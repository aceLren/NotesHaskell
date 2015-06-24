import Prelude hiding (reverse)

-- Article: https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- Distributivity law
prop_reverse_distributivity :: [Int] -> [Int] -> Bool
prop_reverse_distributivity xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_reverse_oops :: [Int] -> [Int] -> Bool
prop_reverse_oops xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys

-------------- Try these ------
-- verboseCheck prop_commutative_add
-- quickCheckWith stdArgs { maxSuccess = 500 } prop_commutative_add
prop_commutative_add :: Integer -> Integer -> Bool
prop_commutative_add x y = x + y == y + x
