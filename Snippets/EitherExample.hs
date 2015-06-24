-- Either definition
-- data  Either a b  =  Left a | Right b
--   deriving (Eq, Ord, Read, Show, Typeable)

-- instance Functor (Either a) where
--     fmap _ (Left x) = Left x
--     fmap f (Right y) = Right (f y)

-- instance Monad (Either e) where
--     ...

onlyEvens :: Int -> Either String Int
onlyEvens x = if even x then return x else Left "It wasn't even"

greaterTen :: Int -> Either String Int
greaterTen x = if x > 10 then return x else Left "It was smaller than 10"

closeRound :: Double -> Either String Int
closeRound x = if err < 0.2 then return val else Left "Too much error to round"
    where
        val = floor x
        err = x - fromIntegral val

alwaysWrong :: Either String Int
alwaysWrong = Left "Just wrong"

doEitherComp :: Double -> Either String Int
doEitherComp x = do 
    a <- closeRound x 
    b <- greaterTen a
    c <- onlyEvens b
    alwaysWrong
    return (c * 12)
