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
        doit2
    )

-- PID Gains
kp :: Double
kp = 0.0100
ki :: Double
ki = 0.0009
kd :: Double
kd = 0.0000

instance HasSensor SimState Double where
    getSensorVal = fmap Sensorval speed
    controlActuator = torque

run_ :: PA SimState Double () -> Sensorval Double -> SimState ()
run_ = run

apply_ :: (Double, Double, Double) -> PA SimState Double ()
apply_ = apply

go :: Double -> SimState ()
go sp = run_ (apply_ (kp, ki, kd) :: PA SimState Double ()) (Sensorval sp)

main :: IO ()
main = print $ runSim (go 100)