{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Control.Monad

newtype Time d = Time d deriving (Eq, Ord)

duration :: Num d => Time d -> Time d -> d
duration (Time d1) (Time d2) = d1-d2

shift :: Num d => Time d -> d->  Time d
shift (Time d1) d2 = Time (d1 + d2)

class (Ord d, Num d, Monad m, Monad t) => TimedMonad m d t | t->m, t->d where
    now :: t (Time d)
    drift :: t d
    delay :: d-> t ()
    lift :: m a -> t a
    run :: t a -> m a

class (Ord d, Num d, Monad m) => HasTimer m d where
    getRealTime :: m (Time d)
    waitUntil :: Time d -> m ()
    getDrift :: Time d -> m d
    getDrift t = do {r <- getRealTime; return (duration r t)}

newtype Micro = Micro Int deriving (Show, Eq, Ord, Num)

getSystemTime :: IO (Time Micro)
getSystemTime = return (Time (Micro 10))

instance HasTimer IO Micro where
    getRealTime = getSystemTime
    waitUntil (Time d) = return ()

data TA m d a  = TA (Time d -> m (Time d, a))

instance (Monad m, HasTimer m d, Applicative (TA m d)) => Monad (TA m d) where
    return a = TA (\s -> return (s,a))
    TA m >>= f = TA ( \s -> m s >>= \(s1,a) -> let (TA m1) = f a in m1 s1)

instance (Monad m, HasTimer m d, Applicative (TA m d)) => TimedMonad m d (TA m d) where
    now = TA (\s -> return (s,s))
    drift = TA $ \s -> getDrift s >>= \d -> return (s,d)
    delay d | d <= 0 = return ()
    delay d | d > 0 = TA $ \s -> do
        {dr <- getDrift s; waitUntil (shift s (d - dr)); return (shift s d, ())}
    lift m = TA $ \s -> m >>= \a -> return (s,a)
    run (TA m) = getRealTime >>= m >>= \ (_,a) -> return a


main = putStrLn "hello World"