import Test.QuickCheck

-- This is how you write the type and the bind for Writer monad
newtype Writer w a = Writer { runWriter :: (a,w) }

-- instance Functor (Writer w) where
--     ...

-- instance Applicative (Writer w) where
--     ...

-- instance (Monoid w) => Monad (Writer w) where
--     ...

-- Problem:
-- Lets assume we want to log operations as we go.  One way to do it
-- is to pass the log along to every function and then return it with
-- the result like this:

type Log = [String]

logOp' :: Int -> Log -> (Int, Log)
logOp' x l = (x,l `mappend` ["Got number: " ++ show x])

tell' :: Log -> Log -> ((),Log)
tell' l lg = ((),lg `mappend` l)

multiplyAndLog' :: Int -> (Int, Log)
multiplyAndLog' vl =
    let
        (newVal, newLog)   = logOp' vl []
        (nextVal, nextLog) = logOp' (vl * 3) newLog
        ((), lastLog)      = tell' ["Sweet, we're done"] nextLog
    in
        (newVal * nextVal, lastLog)

-- Create a Monad, Writer, which lets you do the above implicitly.
-- Instead of passing around the log, you can ignore it.  If you do
-- it right the functions below will work and the tests below them
-- will pass!

tell :: a -> Writer a ()
tell val = Writer ((),val)
-- ask :: Writer (b,a)
-- ask = k

logOp :: Int -> Writer [String] Int
logOp x = Writer (x, ["Got number: " ++ show x])

multiplyAndLog :: Int -> Writer [String] Int
multiplyAndLog vl = do
    a <- logOp vl
    b <- logOp (vl * 3)
    tell ["Sweet, we're done"]
    return (a * b)


prop_same_as_writer :: Int -> Bool
prop_same_as_writer x = multiplyAndLog' x == (runWriter $ multiplyAndLog x)

-- Test with one value, that's pretty cool
test = multiplyAndLog' 100 == (runWriter $ multiplyAndLog 100)

-- But what about with 500 random ones?!?!
qtest = quickCheckWith stdArgs { maxSuccess = 500 } prop_same_as_writer


----------------------------------------------------------
-- My Answer below
----------------------------------------------------------

instance Functor (Writer w) where
    fmap f (Writer (val,writeTo)) = Writer (f val, writeTo)

instance Monoid w => Applicative (Writer w) where
    pure val = Writer (val, mempty)
    Writer (fval,writeToA) <*> Writer (val,writeToB) = Writer (fval val, writeToA `mappend` writeToB)

instance Monoid w => Monad (Writer w) where
    return = pure
    Writer (val, writeToA) >>= f = let Writer (newVal,writeToB) = f val in Writer (newVal, writeToA `mappend` writeToB)
