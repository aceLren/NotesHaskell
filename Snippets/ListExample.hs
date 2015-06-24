-- FROM BASE
-- instance Functor [] where
--     fmap = map

-- -- See Note: [List comprehensions and inlining]
-- instance Applicative [] where
--     pure x    = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]
--     xs *> ys  = [y | _ <- xs, y <- ys]

-- -- See Note: [List comprehensions and inlining]
-- instance Monad []  where
--     xs >>= f             = [y | x <- xs, y <- f x]
--     (>>) = (*>)
--     return x            = [x]
--     fail _              = []

norm :: Double -> Double -> Double
norm x y = sqrt (x ^ 2 + y ^ 2)

norm'' :: [Double] -> [Double] -> [Double]
norm'' xs ys = [norm x y | x <- xs, y <- ys]

-- vectorize?
norm' :: [Double] -> [Double] -> [Double]
norm' xs ys = do
    x <- xs
    y <- ys
    return $ norm x y
    -- z <- return $ norm x y
    -- return (z + 3.0)
    -- or [norm x y]

normPair :: [Double] -> [Double] -> [Double]
normPair xs ys = zipWith norm xs ys 