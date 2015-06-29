import Prelude hiding (Either(..))

data Either a b = Left a | Right b
    deriving (Eq, Ord, Read, Show)

-- Either definition

-- instance Functor (Either a) where
--     ...

-- instance Applicative (Either a) where
--     ...

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
    -- alwaysWrong
    return (c * 12)

test = doEitherComp 100

-----------------------------------------
-- My answer below
-----------------------------------------

-- instance Functor (Either a) where
--     fmap f (Right x) = Right (f x)
--     fmap _ (Left x) = Left x
--
-- instance Applicative (Either a) where
--     pure = Right
--     Right f <*> Right a = Right (f a)
--     Right f <*> Left m = Left m
--     Left m <*> _ = Left m
--
-- instance Monad (Either a) where
--     return = pure
--     Right x >>= f = f x
--     Left m >>= _ = Left m
