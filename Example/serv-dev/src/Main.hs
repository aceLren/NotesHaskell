{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}

import Codec.Xlsx
import Control.Lens
import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TVar    (TVar, newTVar, readTVar,
                                                 writeTVar)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger           (runStderrLoggingT)
import           Control.Monad.Trans.Control    (MonadBaseControl (..))
import           Data.Monoid
import           Data.Text
import          Data.Text.Encoding (encodeUtf8)
import           Data.Time                      (UTCTime, getCurrentTime)
import           Database.Persist               (insert)
import           Database.Persist.Sqlite        (ConnectionPool, SqlPersistT,
                                                 runMigration, runSqlPool,
                                                 withSqlitePool)
import           Database.Persist.TH            (mkMigrate, mkPersist,
                                                 persistLowerCase, share,
                                                 sqlSettings)
import           Network.Wai.Application.Static (defaultWebAppSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setPort)
-- import           Network.Wai.Handler.WarpTLS    (runTLS, TLSSettings(..), tlsSettings)
import qualified Network.Wai.Handler.WebSockets as WS
import           Network.Wai.Middleware.Cors    (simpleCors)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Connection  as WS
import           Thrift.WSServer
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

import           Serv
import qualified Serv_Iface                     as SI
import qualified Serv_Types                     as ST

type People = [Text]
data Global = Global { pool :: ConnectionPool, ppl :: TVar People }

type Dollars = Double
data Food = BEEF | CHK | FISH | HAM | MCH | MTL | SPG | TUR deriving (Show, Ord, Eq)
data Nutrition = Nutrition { food :: Food, cost :: Dollars, a :: Int, c :: Int, b1 :: Int, b2 :: Int } deriving (Show)

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
    av <- decimal
    sep
    cv <- decimal
    sep
    b1v <- decimal
    sep
    b2v <- decimal
    return $ Nutrition f 0.0 av cv b1v b2v

parseNutrition :: Parser [Nutrition]
parseNutrition = do
    manyTill anyChar endOfLine
    many $ parseNutritionField <* endOfLine

-- Example data type that can be stored in the db
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MyData
    name Text
    numb Int
    time UTCTime Maybe
|]

nobody :: [Text]
nobody = []

class DBer a where
    doDB :: MonadBaseControl IO m => a -> SqlPersistT m b -> m b

instance DBer Global where
    doDB app action = runSqlPool action (pool app)

instance SI.Serv_Iface Global where
    ping self = do
        now <- getCurrentTime

        -- Example database insert
        let db = doDB self
        db $ insert $ MyData "my" 2 (Just now)

        print "PING!"

    myOp self sargs msg = do
        print $ "ServArgs: " <> show sargs
        print $ "Message: " <> msg
        let retVal = 32
        print $ "Sending back " <> show retVal
        return retVal

-- | Excel helpers --------------------------------
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

readPrices :: Xlsx -> [(Food, Double)]
readPrices xl = maybeUntil (priceFromExcel xl) [0..]

priceFromExcel :: Xlsx -> Int -> Maybe (Food, Double)
priceFromExcel xl i
    | Just textFood  <- cellT $ xl ^? ixSheet "Prices" . ixCell (base + i, 1) . cellValue . _Just
    , Right food     <- parseOnly foodParser (encodeUtf8 textFood)
    , Just price     <- cellD $ xl ^? ixSheet "Prices" . ixCell (base + i, 3) . cellValue . _Just
    = Just $ (food, price)
    | otherwise = Nothing
    where
        base = 1

main :: IO ()
main = do
    csv <- B.readFile "VitaminContent.csv"
    let nutrition = parseOnly parseNutrition csv
    print nutrition

    xl <- L.readFile "prices.xlsx"
    let prices = readPrices (toXlsx xl)
    print prices

    runStderrLoggingT $ withSqlitePool "resources/db/example.db" 5 $ \pool -> do
        -- Automatically build my database
        runSqlPool (runMigration migrateAll) pool

        st <- liftIO $ atomically $ newTVar nobody
        let glbl = Global pool st
            app = staticApp $ defaultWebAppSettings "web"
            wsMiddle = WS.websocketsOr WS.defaultConnectionOptions (wsHandler glbl Serv.process)

        liftIO
            $ runSettings (setPort 8080 defaultSettings)
            $ (wsMiddle . simpleCors) app

        -- Example TLS version (uncomment WarpTLS above and generate your certs using openssl)
        -- liftIO $ runTLS (tlsSettings "resources/certs/server.crt" "resources/certs/server.key")
        --     { tlsWantClientCert = False
        --     } (setPort 8080 defaultSettings) $ (wsMiddle . simpleCors) app
