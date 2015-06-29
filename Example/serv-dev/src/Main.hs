{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Prelude as P
import           Codec.Xlsx
import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TVar      (TVar, newTVar, readTVar,
                                                   writeTVar)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Logger             (runStderrLoggingT)
import           Control.Monad.Trans.Control      (MonadBaseControl (..))
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy as LT
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Time                        (UTCTime, getCurrentTime)
import           Database.Persist                 (insert)
import           Database.Persist.Sqlite          (ConnectionPool, SqlPersistT,
                                                   runMigration, runSqlPool,
                                                   withSqlitePool)
import           Database.Persist.TH              (mkMigrate, mkPersist,
                                                   persistLowerCase, share,
                                                   sqlSettings)
import           Network.Wai.Application.Static   (defaultWebAppSettings,
                                                   staticApp)
import           Network.Wai.Handler.Warp         (defaultSettings, runSettings,
                                                   setPort)
-- import           Network.Wai.Handler.WarpTLS    (runTLS, TLSSettings(..), tlsSettings)
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy             as L
import qualified Network.Wai.Handler.WebSockets   as WS
import           Network.Wai.Middleware.Cors      (simpleCors)
import qualified Network.WebSockets               as WS
import qualified Network.WebSockets.Connection    as WS
import           Thrift.WSServer
import qualified Data.Vector as V
import Foreign.C
import CPLEX.Param
import CPLEX
import Data.Maybe

import           Serv
import qualified Serv_Iface                       as SI
import qualified Serv_Types                       as ST

import Food

-------------------------------------------------
-- Example types for our server
-------------------------------------------------

type People = [Text]
data Global = Global { pool :: ConnectionPool, ppl :: TVar People }

nobody :: [Text]
nobody = []

-------------------------------------------------
-- Example data for our LP using Persistent so we
-- get DB storage for free!!
-------------------------------------------------

type Dollars = Double
-- This is defined with the template haskell stuff in the file Food.hs
-- data Food = BEEF | CHK | FISH | HAM | MCH | MTL | SPG | TUR deriving (Show, Ord, Eq)

-- Below is template haskell for this data type so I can easily
-- store it in a database
---------
-- data Nutrition = Nutrition { food :: Food, cost :: Dollars, a :: Int, c :: Int, b1 :: Int, b2 :: Int } deriving (Show)
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Nutrition
    food Food
    cost Dollars
    a Double
    c Double
    b1 Double
    b2 Double
    time UTCTime Maybe
    deriving Show Read
|]

-------------------------------------------------
-- Example parsing (including manually doing csv)
-------------------------------------------------

foodParser :: Parser Food
foodParser =
        (string "BEEF"  >> return BEEF)
    <|> (string "CHK"   >> return CHK)
    <|> (string "FISH"  >> return FISH)
    <|> (string "HAM"   >> return HAM)
    <|> (string "MCH"   >> return MCH)
    <|> (string "MTL"   >> return MTL)
    <|> (string "SPG"   >> return SPG)
    <|> (string "TUR"   >> return TUR)

parseNutritionField :: Parser Nutrition
parseNutritionField = do
    let sep = skipSpace >> char ',' >> skipSpace
    f <- foodParser
    sep
    av <- double
    sep
    cv <- double
    sep
    b1v <- double
    sep
    b2v <- double
    return $ Nutrition f 0.0 av cv b1v b2v Nothing

parseNutrition :: Parser [Nutrition]
parseNutrition = do
    manyTill anyChar endOfLine
    many $ parseNutritionField <* endOfLine

-------------------------------------------------
-- Server Instance!
-------------------------------------------------

class DBer a where
    doDB :: MonadBaseControl IO m => a -> SqlPersistT m b -> m b

instance DBer Global where
    doDB app action = runSqlPool action (pool app)

instance SI.Serv_Iface Global where
    ping self = do
        now <- getCurrentTime

        -- Example database insert
        let db = doDB self
        db $ insert $ Nutrition FISH 2.4 1 2 3 4 (Just now)

        print "PING!"

    myOp self sargs msg = do
        print $ "ServArgs: " <> show sargs
        print $ "Message: " <> msg
        let retVal = 32
        print $ "Sending back " <> show retVal
        return retVal

    saveFood self nm cst a c b1 b2 = do
        print $ "Got " <> nm
        now <- getCurrentTime
        case (parseOnly foodParser (encodeUtf8 $ LT.toStrict nm)) of
            Left _ -> return False
            Right f -> do
                doDB self $ insert $ Nutrition f cst a c b1 b2 (Just now)
                print $ "Saved food"
                return True

-------------------------------------------------
-- Excel helpers
-------------------------------------------------

cellT :: Maybe CellValue -> Maybe Text
cellT (Just (CellText t)) = Just t
cellT _ = Nothing

cellD :: Maybe CellValue -> Maybe Double
cellD (Just (CellDouble d)) = Just d
cellD _ = Nothing

cellB :: CellValue -> Maybe Bool
cellB (CellBool b) = Just b
cellB _ = Nothing

maybeUntil :: (a -> Maybe b) -> [a] -> [b]
maybeUntil _ [] = []
maybeUntil a (x:xs) = case (a x) of
    Nothing -> []
    Just v -> v : maybeUntil a xs

-------------------------------------------------
-- Read from Excel
-------------------------------------------------
readPrices :: Xlsx -> [(Food, Dollars)]
readPrices xl = maybeUntil (priceFromExcel xl) [0..]

priceFromExcel :: Xlsx -> Int -> Maybe (Food, Dollars)
priceFromExcel xl i
    | Just textFood  <- cellT $ xl ^? ixSheet "Prices" . ixCell (base + i, 1) . cellValue . _Just
    , Right food     <- parseOnly foodParser (encodeUtf8 textFood)
    , Just price     <- cellD $ xl ^? ixSheet "Prices" . ixCell (base + i, 3) . cellValue . _Just
    = Just $ (food, price)
    | otherwise = Nothing
    where
        base = 1

-------------------------------------------------
-- CPLEX SOLVER STUFF
-------------------------------------------------

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

genNutritionMatrix :: [Nutrition] -> [(Row,Col,Double)]
genNutritionMatrix nutrition = P.concat [
    [(Row 0, Col x, nutritionA n)
    ,(Row 1, Col x, nutritionC n)
    ,(Row 2, Col x, nutritionB1 n)
    ,(Row 3, Col x, nutritionB2 n)
    ] | (x,n) <- idexed]
    where
        idexed = P.zip [0..] nutrition

type NutritionConstraints = [Sense]
type FoodConstraints = [(Maybe Double, Maybe Double)]

sol' :: NutritionConstraints -> FoodConstraints -> [Nutrition] -> IO ()
sol' rhs xbnds nutrition = withEnv $ \env -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "Nutrition" $ \lp -> do
    let objsen = CPX_MIN
        obj = P.map nutritionCost nutrition
        amat = genNutritionMatrix nutrition
    statusLp <- copyLp env lp objsen (V.fromList obj) (V.fromList rhs) amat (V.fromList xbnds)

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    statusOpt <- qpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg

    statusSol <- getSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do
        putStrLn $ "x      : " ++ show (solX sol)
        putStrLn $ "pi'    : " ++ show (solPi sol)
        putStrLn $ "slack  : " ++ show (solSlack sol)
        putStrLn $ "dj     : " ++ show (solDj sol)
        putStrLn $ "solstat: " ++ show (solStat sol)
        putStrLn $ "objval : " ++ show (solObj sol)

fillInPrices :: M.Map Food Dollars -> [Nutrition] -> [Maybe Nutrition]
fillInPrices prices [] = []
fillInPrices prices (x:xs) = case M.lookup (nutritionFood x) prices of
    Nothing -> fillInPrices prices xs
    Just cost -> Just (x { nutritionCost = cost }) : fillInPrices prices xs

nutrientCount :: Int
nutrientCount = 4

nutritionConstraints :: Nutrition -> (Double -> Sense) -> [Sense]
nutritionConstraints n sc = P.map (\x -> sc $ x n) [nutritionA, nutritionC, nutritionB1, nutritionB2]

main :: IO ()
main = do
    -- Read files for solving
    csv <- B.readFile "VitaminContent.csv"
    let rawNutrition = parseOnly parseNutrition csv
    xl <- L.readFile "prices.xlsx"
    let prices = M.fromList $ readPrices (toXlsx xl)

    -- Fuse data
    now <- getCurrentTime
    nutrition <- case rawNutrition of
        Right n -> do
            let tmp = fromMaybe [] $ sequence $ fillInPrices prices n
            return $ fmap (\x -> x { nutritionTime = Just now }) tmp
        Left s -> do
            print $ "Error " <> s
            return []

    -- Build constraints (we'll just say food has to be non-zero and
    -- all nutrition has to be a minimum of 700)
    print nutrition
    let nutritionMinimums = Nutrition ALL 0.0 700 700 700 700 (Just now)
        foodCount = P.length nutrition
        foodConstraints = P.replicate foodCount (Just 0, Nothing)

    -- Our only constraint on food is non-zero
    sol' (nutritionConstraints nutritionMinimums G)
         foodConstraints
         nutrition
         >>= print

    -- Now prep for server with a pool of database connections
    runStderrLoggingT $ withSqlitePool "resources/db/example.db" 5 $ \pool -> do
        -- Automatically build / migrate my database -> at one point a,c,b1,b2
        -- were Ints, this made the schema migration automatic
        runSqlPool (runMigration migrateAll) pool

        -- We can go ahead and insert everything from our problem above
        -- if we want
        mapM (\x -> runSqlPool (insert x) pool) nutrition

        -- Prepare the server / handler
        st <- liftIO $ atomically $ newTVar nobody
        let glbl = Global pool st
            app = staticApp $ defaultWebAppSettings "web"
            wsMiddle = WS.websocketsOr WS.defaultConnectionOptions (wsHandler glbl Serv.process)

        -- Serve
        liftIO
            $ runSettings (setPort 8080 defaultSettings)
            $ (wsMiddle . simpleCors) app

        -- Example TLS version (uncomment WarpTLS above and generate your certs using openssl)
        -- liftIO $ runTLS (tlsSettings "resources/certs/server.crt" "resources/certs/server.key")
        --     { tlsWantClientCert = False
        --     } (setPort 8080 defaultSettings) $ (wsMiddle . simpleCors) app
