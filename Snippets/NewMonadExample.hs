import Prelude hiding ((>>=), return)

-- This is a backtracking constraint monad:
-- FROM http://www.randomhacks.net/2007/03/12/monads-in-15-minutes/

type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

pair456 :: Int -> Choice (Int, Int)
pair456 x = choose [(x,4),(x,5),(x,6)]

join :: Choice (Choice a) -> Choice a
join = concat 

(>>=) :: Choice a -> (a -> Choice b) -> Choice b
choices >>= f = join (map f choices)

return :: a -> Choice a
return x = choose [x]

makePairs :: Choice (Int, Int)
makePairs =
    choose [1,2,3] >>= (\x ->
    choose [4,5,6] >>= (\y ->
    return (x,y)))

makePairs' :: Choice (Int, Int)
makePairs' = do
    x <- choose [1,2,3]
    y <- choose [4,5,6]
    -- mzero
    return (x,y)

mzero :: Choice a
mzero = choose []

guard :: Bool -> Choice ()
guard True = return ()
guard False = mzero

-- https://en.wikipedia.org/wiki/Monad_(functional_programming)
-- binding mzero results in mzero
solveConstraint = do
    x <- choose [1,2,3]
    y <- choose [4,5,6]
    guard (x*y == 8)
    return (x,y)

