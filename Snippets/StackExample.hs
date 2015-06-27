import Test.QuickCheck
-- To see real definitions look for StateT's instance declarations in:
-- http://hackage.haskell.org/package/transformers-0.4.3.0/docs/src/Control-Monad-Trans-State-Lazy.html

-- This example is borrowed from http://learnyouahaskell.com/for-a-few-monads-more

-- To try it, I go into the Snippets folder and type:
-- cabal sandbox init
-- cabal install quickcheck
-- cabal repl
-- ghci> main

-- The cabal repl stage should fail if you haven't implemented the type classes below

-- This is meant as a HW exercies: implement Functor, Applicative, and Monad for:
newtype State s a = State { runState :: s -> (a,s) }
-- In my solution below I'll refer to the function s -> (a,s) as stateFunc.  It takes
-- a state and returns the tuple (value, state).  Notice the left
-- is a TYPE constructor, s and a are building the type, whereas
-- on the right we have the DATA constructor.  State, which takes
-- one argument (the function s -> (a,s)) which is named runState
-- so if we get a State v, we can say runState (State v) and get
-- s -> (a,s) out.

-- instance Functor (State s) where
--     ...
-- instance Applicative (State s) where
--     ...
-- instance Monad (State s) where
--     ...


-----------------------------------------
-- Now with that we can try it on this stack
-- data type.  If you've correctly made Stack
-- a monad, the test function below will equal
-- true
-- (My answer is at the bottom)
-----------------------------------------

type Stack = [Int]

-- FIRST what does this look like without State monad?
pop :: Stack -> (Int, Stack)
pop [] = (-1, [])
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((),newStack1) = push 17 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

-- like get, but we're modifying state
pop' :: State Stack Int
-- pop' = State $ \(x:xs) -> (x,xs)
pop' = State pop

-- Like put, but we're adding to state
push' :: Int -> State Stack ()
-- push' x = State $ \xs -> ((),x:xs)
push' a = State $ push a

stackManip' :: State Stack Int
stackManip' = do
    push' 17
    pop'
    a <- pop'
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,2,5]
        else put stackNow
    return a

-- If you've done it right, this is true
test :: Bool
test = runState stackManip' [41,2,3] == stackManip [41,2,3]

-- Or we can get fancy and do a quickcheck!
prop_same_pop :: [Int] -> [Int] -> Bool
prop_same_pop xs ys = runState stackManip' (xs ++ ys) == stackManip (xs ++ ys)

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 500 } prop_same_pop

get = undefined
put = undefined

-------------------------------------------
-- My ANSWER IS BELOW
------------------------------------------

-- instance Functor (State s) where
--     fmap mappedFunc (State stateFunc) = State $ \currentState ->
--         let (currentValue, newState) = stateFunc currentState
--         in (mappedFunc currentValue, newState)
--
-- -- In definition (<*>) = ap .  Runs as monad and retrieves the first
-- -- applied to teh second:
-- -- ap                :: (Monad m) => m (a -> b) -> m a -> m b
-- -- ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- instance Applicative (State s) where
--     pure x = State $ \constantState -> (x, constantState)
--     (State a) <*> (State b) = State $ \state ->
--         let (a1, newState) = a state
--             (b1, finalState) = b newState
--         in (a1 b1, finalState)
--
-- instance Monad (State s) where
--     return = pure
--     (State stateFunc) >>= newMaker = State $ \currentState ->
--         let (newValue,newState) = stateFunc currentState
--             (State newStateFunc) = newMaker newValue
--         in newStateFunc newState
--
-- -- Get it in the value, keep state going on
-- get = State $ \s -> (s,s)
-- -- Forget about the value, cram the state in there
-- put s = State $ \_ -> ((),s)
