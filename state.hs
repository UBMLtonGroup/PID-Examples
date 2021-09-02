import Control.Monad.State.Lazy
import Data.Maybe

-- PID Gains
kp :: Double
kp = 0.0100
ki :: Double
ki = 0.0009
kd :: Double
kd = 0.0000

-- Flywheel Moment of Intertia
i :: Double
i = 1/2 * 0.1 * 0.05**2 -- for 100 g, 10 cm diameter flywheel

-- Drag coefficient
c :: Double
c = 0.001

-- timestep
dt :: Double
dt = 0.01 -- s

-- number of iterations
n :: Int
n = 1000

-- Controller setpoint
setpoint :: Double
setpoint = 100 -- rad/s

type SimulationState = (Double, Double)

sim_init :: SimulationState
sim_init = (0.0, 0.0)

type Gains = (Double, Double, Double)
type ControllerState = (Gains, Double, Double, Double)

controller_init :: Double -> ControllerState
controller_init s = ((kp, kd, ki), s, 0.0, 0.0) 

s0 :: Double -> (ControllerState, SimulationState)
s0 set = (controller_init set, sim_init)

update :: State (ControllerState, SimulationState) Double
update = (get >>= \(((kp, kd, ki), s, d, i), (theta, omega)) -> let err = s - omega in put (((kp, kd, ki), s, err, i+err), (theta, omega)) >>= \() -> get >>= \(((kp, kd, ki), s, d, i), (_, omega)) -> let err = s - omega in return (kp * err + ki * (i+err) + kd * (d-err))) :: State (ControllerState, SimulationState) Double

simulate :: Double -> State (s, SimulationState) ()
simulate = (\tau -> get >>= \(cs, (theta, omega)) -> let alpha = 1.0/i * (tau - c * omega) in put (cs, (theta + omega * dt, omega + alpha * dt))) :: Double -> State (s, SimulationState) ()

:{
clip :: (Ord p, Fractional p) => p -> p
clip x  | x > 1.0 = 1.0
        | x < -1.0 = -1.0
        | otherwise = x
:}

:{
run :: Int -> State (ControllerState, SimulationState) Double
run 0 = get >>= \(_, (_, omega)) -> return omega :: State (ControllerState, SimulationState) Double
run n = update >>= \tau -> return (clip tau) >>= simulate >>= \_ -> run (n-1)
:}