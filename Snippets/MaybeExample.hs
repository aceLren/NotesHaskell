-- FROM BASE
-- instance  Functor Maybe  where
--     fmap _ Nothing       = Nothing
--     fmap f (Just a)      = Just (f a)

-- instance Applicative Maybe where
--     pure = Just

--     Just f  <*> m       = fmap f m
--     Nothing <*> _m      = Nothing

--     Just _m1 *> m2      = m2
--     Nothing  *> _m2     = Nothing

-- instance  Monad Maybe  where
--     (Just x) >>= k      = k x
--     Nothing  >>= _      = Nothing

--     (>>) = (*>)

--     return              = Just
--     fail _              = Nothing

half :: Integral a => a -> Maybe a
half x = if even x then Just (x `div` 2) else Nothing

example :: Maybe Int
example = half 160 >>= half >>= half >>= half >>= half

exampleDo :: Maybe Int
exampleDo = do
    w <- half 160
    x <- half w
    y <- half x
    z <- half y
    half z

iAmOdd :: Int -> Maybe String
iAmOdd x = if odd x then Just "yep" else Nothing

-- exampleDo >>= half >>= iAmOdd