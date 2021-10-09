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
        rep
    )

-- PID Gains
kp :: Double
kp = 0.011
ki :: Double
ki = 0.001
kd :: Double
kd = 0.000

instance HasSensor SimState Double where
    getSensorVal = fmap Sensorval speed
    controlActuator = torque

controller :: PA SimState Double ()
controller = apply (kp, ki, kd)

go :: Double -> SimState ()
go sp = run (rep 10 controller) (Sensorval sp)

main :: IO ()
main = print $ runSim (go 100)