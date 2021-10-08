{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
import Flywheel ( SimState, speed, torque, runSim )
import PIDmonad ( 
        HasSensor(getSensorVal, controlActuator),
        Sensorval(Sensorval),
        PIDMonad(apply, run),
        PA, PIDMonad,
        doit2, runTo, rep
    )

-- PID Gains
kp :: Double
kp = 0.0120
ki :: Double
ki = 0.0010
kd :: Double
kd = 0.0000

instance HasSensor SimState Double where
    getSensorVal = fmap Sensorval speed
    controlActuator = torque

apply_ :: (Double, Double, Double) -> PA SimState Double ()
apply_ = apply

go :: Double -> SimState ()
go sp = run (rep 10 (apply_ (kp, ki, kd) :: PA SimState Double ())) (Sensorval sp)

main :: IO ()
main = print $ runSim (go 100)