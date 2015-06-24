module Greg where

greg :: Int -> Double
greg x = 4 * (-1) ^ (x + 1) / (2.0 * k - 1)
    where k = fromIntegral x
