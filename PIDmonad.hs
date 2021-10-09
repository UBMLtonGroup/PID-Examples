{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module PIDmonad where
import Control.Monad
import Control.Applicative (Applicative)
import Control.Monad.Fix

import Debug.Trace ( trace )

newtype Sensorval v = Sensorval v deriving (Eq, Ord)

diff :: Num v => Sensorval v -> Sensorval v -> v
diff (Sensorval v1) (Sensorval v2) = v2 - v1

class (Num v, Monad m, Monad p) => PIDMonad m v p | p->m, p->v where
    current :: p (Sensorval v)
    setSP ::  v -> p ()
    drift :: p v
    control :: (v,v,v) -> p v
    apply :: (v,v,v) -> p ()
    lift :: m a -> p a
    run :: p a ->Sensorval v ->  m a


class (Ord v , Num v, Monad m) => HasSensor m v where
    getSensorVal :: m (Sensorval v)
    controlActuator ::  v -> m ()
    getDiff :: Sensorval v -> m v
    getDiff v = do
        {r <- getSensorVal; return (diff r v)}

getSensorReadingA :: IO (Sensorval Double)
getSensorReadingA = return (Sensorval 0.0)

getSensorReadingB :: IO (Sensorval Double)
getSensorReadingB = return (Sensorval 1.0)

controlDevice ::   Double -> IO ()
controlDevice v = print v

instance HasSensor IO Double where
    getSensorVal = getSensorReadingA
    controlActuator = controlDevice

data PS v = PS {integral_err :: v, deriv_err :: v, target :: Sensorval v}
newtype PA m v a = PA (PS v -> m (PS v , a))

instance (Monad m, HasSensor m v) => Functor (PA m v) where
    fmap = liftM

instance (Monad m, HasSensor m v) => Applicative (PA m v) where
    pure a = PA (\s -> return (s,a))
    (<*>) = ap

instance (Monad m, HasSensor m v) => Monad (PA m v) where
    PA m >>= f = PA (\s -> m s >>= \(s1,a) -> let (PA m1) = f a in m1 s1)

instance (Monad m, HasSensor m v, Applicative (PA m v)) => PIDMonad m v (PA m v) where
    current = PA $ \s -> let PS _ _ v = s in return (s,v)
    setSP n = PA $ \s -> let PS i d _ = s in return (PS i d (Sensorval n), ())
    drift = PA $ \s -> let PS i d v = s in getDiff v >>= \e -> return (s , e)
    control (kp, ki, kd) = PA $ \s -> let PS _ _ v = s in getDiff v >>= \e -> let PS i d v = s in
        return (PS (i+e) e v, (kp*e)+(ki*(i+e))+(kd*(e-d)))
    --control (kp, ki, kd) = PA $ \s -> let (PA m1) = drift in m1 s >>= \ (PS i d v, e) -> return (PS i d v , (kp*e)+(ki*i)+(kd*d))
    apply (kp, ki, kd) = PA $ \s -> let (PA m1) = control (kp,ki,kd) in m1 s >>= \(s1,c) -> controlActuator c >>= \a -> return (s1,a)
    lift m = PA $ \s -> m >>= \a -> return (s,a)
    run (PA m) sp = snd <$> m (PS 0 0 sp)
    --run (PA m) sp = let loop s = do { (ss,_) <- m s; trace "." loop ss} in loop (PS 0 0 sp)
    --run (PA m) sp = flip fix (1,PS 0 0 sp) $ \loop (n,s) -> do {(ss,a)<- m s; putStrLn "hi" >>  loop (n+1,ss)}
    --run = runTo 0

{- 
step :: (Integral n, Monad m, HasSensor m v) => n -> PA m v a -> PS v -> m (PS v, a)
step 0 (PA m) s = m s
step n (PA m) s = step (n-1) (PA m) s >>= m . fst

runTo :: (Integral n, HasSensor m v) => n -> PA m v a -> Sensorval v -> m a
runTo n a sp = step n a (PS 0 0 sp) >>= return . snd
 -}
rep :: (Integral n, Monad m, HasSensor m v) => n -> PA m v () -> PA m v ()
rep 1 x = x
rep n a = a *> rep (n-1) a

type PIO = PA IO Double

instance HasSensor PIO Double where
    getSensorVal = lift getSensorReadingB
    controlActuator v = setSP v >>= \() -> trace ("SSP: " ++ show v) secondary

{- runSt :: Monad m => PA m v a -> PS v -> m (PS v, a)
runSt (PA m) s = m s >>= \(s1,a) -> return (s1,a)

execST :: (Monad m, Num v) => PA m v a -> PS v -> m (PS v)
execST p s = liftM fst (runSt p s)

loop :: (HasSensor m v, Applicative (PA m v)) => PS v -> m b
loop s = do
    { ss <- execST (apply (1,2,3)) s; loop ss}

initState :: Num v => Sensorval v -> PS v
initState sp = PS 0 0 sp

doit :: (HasSensor m v, Applicative (PA m v)) => Sensorval v -> m b
doit sp = loop (initState sp) -}

-- ### Using only apply
primary :: PA PIO Double ()
primary = apply (1,2,3)

secondary :: PA IO Double ()
secondary = apply (1,2,3)

cascade :: Sensorval Double -> Sensorval Double -> IO ()
cascade primarySP secondaryInitSP = run (run (rep 10 primary) primarySP) secondaryInitSP

{- -- ## using control and apply
primaryC :: PA IO Double Double
primaryC = control (1,2,3)

secondaryC :: Double -> PA IO Double ()
secondaryC sp= PA $ \s -> let (PA m1) = setSP sp in m1 s >>= \ (s1,_) -> let PA m = apply (1,2,3) in m s1 

cascade :: (Monad m, HasSensor m v) => PA m v a -> (a -> PA m v b) -> PS v -> PS v -> m b
cascade (PA m) f s1 s2 = m s1 >>= \(s1',c) -> let (PA m2) = f c in m2 s2 >>= \(s2',a) -> return a

test = cascade primaryC secondaryC 
 -}

--doit2 :: (Monad m, HasSensor m v) => Sensorval v -> (v, v, v) -> m ()
--doit2 sp g =  run (apply g) sp

main :: IO ()
main = putStrLn "hello World"