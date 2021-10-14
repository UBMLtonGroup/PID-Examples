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

newtype Measurement v = Measurement v deriving (Eq, Ord)

diff :: Num v => Measurement v -> Measurement v -> v
diff (Measurement v1) (Measurement v2) = v2 - v1

class (Num v, Monad m, Monad p) => PIDMonad m v p | p->m, p->v where
    current :: p (Measurement v)
    setSP ::  Measurement v -> p ()
    err :: p v
    apply :: (v,v,v) -> p ()
    lift :: m a -> p a
    run :: Measurement v -> p a ->  m a


class (Ord v , Num v, Monad m) => HasSensor m v where
    getSensorVal :: m (Measurement v)
    controlActuator ::  v -> m ()
    getDiff :: Measurement v -> m v
    getDiff v = do
        {r <- getSensorVal; return (diff r v)}

getSensorReadingA :: IO (Measurement Double)
getSensorReadingA = return (Measurement 1.0)

controlDevice ::   Double -> IO ()
controlDevice v = print v

instance HasSensor IO Double where
    getSensorVal = getSensorReadingA
    controlActuator = controlDevice

data PS v = PS {int :: v, prev :: v, target :: Measurement v}
newtype PA m v a = PA (PS v -> m (PS v , a))

instance (Monad m, HasSensor m v) => Functor (PA m v) where
    fmap = liftM

instance (Monad m, HasSensor m v) => Applicative (PA m v) where
    pure a = PA (\s -> return (s,a))
    (<*>) = ap

instance (Monad m, HasSensor m v) => Monad (PA m v) where
    PA m >>= f = PA (\s -> m s >>= \(s1,a) -> let (PA m1) = f a in m1 s1)

instance (Monad m, HasSensor m v) => PIDMonad m v (PA m v) where
    current = PA $ \s -> let PS _ _ v = s in return (s, v)
    setSP n = PA $ \(PS i d _) -> return (PS i d n, ())
    err = PA $ \s -> 
        let PS _ _ v = s in getDiff v >>= \e -> return (s, e)
    apply (kp, ki, kd) = PA $ \(PS i d v) -> getDiff v >>= \e -> 
        let c = (kp*e)+(ki*(i+e))+(kd*(e-d)) in 
          controlActuator c >>= \a -> return (PS (i+e) e v, a)
    lift m = PA $ \s -> m >>= \a -> return (s, a)
    run sp (PA m) = snd <$> m (PS 0 0 sp)

rep :: (Integral n, Monad m) => n -> m () -> m ()
rep 1 x = x
rep n a = a >>= \() -> rep (n-1) a

type PIO = PA IO Double

pidcontrol :: PIO ()
pidcontrol = apply (1,2,3)

keepDesiredTemp :: Measurement Double -> IO ()
keepDesiredTemp sp = run sp (rep 100 pidcontrol) 