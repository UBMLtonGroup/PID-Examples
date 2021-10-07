module Flywheel where

import Control.Monad.State.Lazy
    ( MonadState(put, get), evalState, State )

import Debug.Trace ( trace ) 

type SimState = State (Double, Double)

-- Flywheel Moment of Intertia
i :: Double
i = 1/2 * 0.1 * 0.05**2 -- for 100 g, 10 cm diameter flywheel

-- Drag coefficient
c :: Double
c = 0.001

-- timestep
dt :: Double
dt = 0.01 -- s

speed :: SimState Double
speed = get >>= \(_, omega) -> return omega

torque :: Double -> SimState ()
torque tau = get >>= \(theta, omega) 
    -> trace (show omega ++ "\n") put (theta + omega * dt, omega + 1.0/i * (tau - c * omega) * dt)

runSim :: SimState () -> Double
runSim s = evalState (s >>= \() -> speed) (0.0, 0.0)