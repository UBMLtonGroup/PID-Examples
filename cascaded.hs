{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import PIDmonad (
        HasSensor(getSensorVal, controlActuator),
        Measurement(Measurement),
        PIDMonad(apply, run,lift,setSP),
        PA, PIDMonad,
        rep
    )
import Debug.Trace ( trace )

getSensorReadingB :: IO (Measurement Double)
getSensorReadingB = return (Measurement 1.0)

type PIO = PA IO Double

instance HasSensor PIO Double where
    getSensorVal = lift getSensorReadingB
    controlActuator v = setSP (Measurement v) >>= \() -> secondary

primary :: PA PIO Double ()
primary = apply (1,2,3)

secondary :: PA IO Double ()
secondary = apply (1,1,1)

cascade :: Measurement Double -> Measurement Double -> IO ()
cascade primarySP secondaryInitSP = run secondaryInitSP (run primarySP (rep 10 primary) ) 

main = cascade (Measurement 2.0) (Measurement  1.0) 