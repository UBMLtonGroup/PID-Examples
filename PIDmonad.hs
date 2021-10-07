{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module PIDmonad where
import Control.Monad
import Control.Applicative (Applicative)

newtype Sensorval v = Sensorval v deriving (Eq, Ord)

diff :: Num v => Sensorval v -> Sensorval v -> v
diff (Sensorval v1) (Sensorval v2) = v1 - v2

class (Num v, Monad m, Monad p) => PIDMonad m v p | p->m, p->v where
    current :: p (Sensorval v)
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

getSensorReading :: IO (Sensorval Double)
getSensorReading = return (Sensorval 10.0)

controlDevice ::   Double -> IO ()
controlDevice v = return ()

instance HasSensor IO Double where
    getSensorVal = getSensorReading
    controlActuator = controlDevice

data PS v = PS {integral_err :: v, deriv_err :: v, target :: Sensorval v}
data PA m v a = PA (PS v -> m (PS v , a))

instance (Monad m, HasSensor m v) => Functor (PA m v) where 
    fmap = liftM

instance (Monad m, HasSensor m v) => Applicative (PA m v) where 
    pure a = PA (\s -> return (s,a))
    (<*>) = ap

instance (Monad m, HasSensor m v) => Monad (PA m v) where
    PA m >>= f = PA (\s -> m s >>= \(s1,a) -> let (PA m1) = f a in m1 s1)

instance (Monad m, HasSensor m v, Applicative (PA m v)) => PIDMonad m v (PA m v) where
    current = PA $ \s -> let PS _ _ v = s in return (s,v)
    drift = PA $ \s -> let PS i d v = s in getDiff v >>= \e -> let PS i d v = s in return (PS (i+e) (e-d) v , e)
    --control (kp, ki, kd) = PA $ \s -> let PS i d v = s in getDiff v >>= \e -> let PS i d v = s in
        --return (PS (i+e) (e-d) v , (kp*e)+(ki*i)+(kd*d))
    control (kp, ki, kd) = PA $ \s -> let (PA m1) = drift in m1 s >>= \ (PS i d v, e) -> return (PS i d v , (kp*e)+(ki*i)+(kd*d))
    apply (kp, ki, kd) = PA $ \s -> let (PA m1) = control (kp,ki,kd) in m1 s >>= \(s1,c) -> controlActuator c >>= \a -> return (s1,a)
    lift m = PA $ \s -> m >>= \a -> return (s,a)
    --run (PA m)  =m (PS 0 0 (Sensorval 1)) >>= \ (_,a) -> return a
    run (PA m) sp = let loop s = do { (ss,_) <- m s; loop ss} in loop (PS 0 0 sp)

type PIO = PA IO Double

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

doit2 sp g =  run (apply g) sp

main :: IO ()
main = putStrLn "hello World"