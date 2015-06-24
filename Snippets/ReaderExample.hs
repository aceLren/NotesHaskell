import Control.Monad.Reader

-- Reader ThingYouRead ResultType
-- newtype Reader r a = Reader { runReader :: r -> a }

-- instance Functor (Reader r) where
--     fmap f m = Reader $ \r -> f (runReader m r)

-- instance Monad (Reader r) where
--     return a = Reader $ \_ -> a
--     m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

type Handler = Reader ServerGlobals (Either String Int)

data ServerGlobals = ServerGlobals
    { users :: [String]
    , dbHandle :: Int
    } deriving (Show)

-- handleConnection :: String -> Reader ServerGlobals (Either String Int)
handleConnection :: String -> Handler
handleConnection newUser = do
    usrs <- asks users
    dhnd <- asks dbHandle 
    if newUser `elem` usrs
        then return (Left $ "Sorry " ++ newUser ++ ", once is enough")
        else return (Right $ length usrs + 1)

ex :: Either String Int 
ex = runReader (handleConnection "Joe") $ ServerGlobals ["Bill","Ted"] 4

exErr :: Either String Int
exErr = runReader (handleConnection "Bill") $ ServerGlobals ["Bill","Ted"] 4