import Control.Monad.State.Lazy

type Stack = [Int]

-- FIRST what does this look like without State monad?
pop :: Stack -> (Int, Stack)
pop [] = (-1, [])
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

-- This is it with the state monad
pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x,xs)

push' :: Int -> State Stack () 
push' x = state $ \xs -> ((),x:xs) 

stackManip' :: State Stack Int 
stackManip' = do
    push' 17 
    a <- pop'
    pop'
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,2,5]
        else put [9,9,9]
    return a