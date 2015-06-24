-- See http://stackoverflow.com/a/9244715/439699 for discussion

instance  Functor IO where
   fmap f x = x >>= (return . f)

instance Applicative IO where
    pure = return
    (<*>) = ap

instance  Monad IO  where
    m >> k    = m >>= \ _ -> k
    return    = returnIO
    (>>=)     = bindIO
    fail s    = failIO s

-- Unboxed tuple - https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/primitives.html#unboxed-tuples
returnIO :: a -> IO a
returnIO x = IO $ \ s -> (# s, x #)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO $ \ s -> case m s of (# new_s, a #) -> unIO (k a) new_s

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO $ \ s -> case m s of (# new_s, _ #) -> unIO k new_s

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a