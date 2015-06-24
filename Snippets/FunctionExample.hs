import Control.Monad.Instances

-- instance Functor ((->) r) where
--     fmap = (.)

-- instance Applicative ((->) a) where
--     pure = const
--     (<*>) f g x = f x (g x)

-- instance Monad ((->) r) where
--     return = const
--     f >>= k = \r -> k (f r) r
    
-- It's passing on the result (f r) and the parameter
-- So all get the same parameter, and each ones result
-- goes forward.

addStuffO :: Int -> Int 
addStuffO = (*2) >>= 
    \x -> (+10) >>= 
    \y -> (`div` 2) >>= 
    \z -> return (x + y + z)

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    c <- (`div` 2)
    return (a + b + c)