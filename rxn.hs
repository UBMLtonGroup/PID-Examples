import Debug.Trace ( trace, traceShowId ) 

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


-- PID Gains
kp :: Double
kp = 0.0100
ki :: Double
ki = 0.0009
kd :: Double
kd = 0.0000

-- Initial state
init_theta = 0.0
init_omega = 0.0
init_int = 0.0
init_err = 0.0

step :: Double -> Double -> Double -> (Double, Double)
step theta omega tau = let
    alpha = 1.0/i * (tau - c * omega)
    in
    (theta + omega * dt, omega + alpha * dt)

controller :: Double -> Double -> Double -> Double -> Double -> (Double, Double, Double)
controller theta omega last_int last_err setpoint =
    let err = setpoint - omega in
    let int = (last_int + err) in -- * dt?
    let d = (last_err - err) in -- / dt?
    let u = kp * err + ki * int + kd * d in
    (u, int, err)

clip :: (Ord p, Fractional p) => p -> p
-- clip x = if x > 1.0 then 1.0 else if x < -1.0 then -1.0 else x
clip x  | x > 1.0 = 1.0
        | x < -1.0 = -1.0
        | otherwise = x 

run :: Double -> Double -> Double -> Double -> Double -> Int -> Double
run int last_err theta omega setpoint 0 = omega
run int last_err theta omega setpoint n =
    let (tau, next_int, err) = controller theta omega int last_err setpoint in
    let (next_theta, next_omega) = step theta omega tau in
    run next_int err next_theta next_omega setpoint (n-1)

simulate :: Double -> Int -> Double
simulate = run init_int init_err init_theta init_omega

result :: Double
result = simulate setpoint n
