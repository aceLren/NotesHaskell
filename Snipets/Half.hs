half :: Integral a => a -> Maybe a
half x = if even x then Just (x `div` 2) else Nothing

example = half 160 >>= half >>= half >>= half >>= half
