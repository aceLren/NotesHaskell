import Control.Monad.Writer

-- This is how you write the type and the bind for Writer monad
-- newtype Writer w a = Writer { runWriter :: (a,w) }

-- instance (Monoid w) => Monad (Writer w) where 
--     return x = Writer (x, mempty)
--     (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

logOp :: Int -> Writer [String] Int
logOp x = writer (x, ["Got number: " ++ show x])

multiplyAndLog :: Int -> Writer [String] Int
multiplyAndLog vl = do
    a <- logOp vl
    b <- logOp (vl * 3)
    tell ["Sweet, we're done"]
    return (a * b)


    