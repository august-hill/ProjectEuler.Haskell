-- Bench.hs - Shared benchmark utility for Project Euler Haskell solutions.
-- Usage:
--   import Bench (runBench)
--   solve :: Integer
--   solve = {- return answer -}
--   main = runBench 60 (return solve)

module Bench (runBench) where

import Data.List (sort)
import Data.IORef
import System.CPUTime
import Control.DeepSeq (NFData, rnf)

-- | Get current time in nanoseconds (from picoseconds)
getNs :: IO Integer
getNs = do
    t <- getCPUTime  -- picoseconds
    return (t `div` 1000)

-- | Run benchmark with warmup, adaptive calibration, and median timing.
runBench :: (NFData a, Show a) => Int -> IO a -> IO ()
runBench problem solveIO = do
    -- Warmup: 3 runs
    mapM_ (\_ -> solveIO >>= \r -> rnf r `seq` return ()) [1..3 :: Int]

    -- Calibrate: time one run
    t0 <- getNs
    r <- solveIO
    rnf r `seq` return ()
    t1 <- getNs
    let calNs = t1 - t0

    let iters | calNs < 1000000     = 1000
              | calNs < 100000000   = 100
              | calNs < 1000000000  = 10
              | otherwise           = 3

    -- Timed runs
    ansRef <- newIORef undefined
    times <- mapM (\_ -> do
        s <- getNs
        ans <- solveIO
        rnf ans `seq` return ()
        e <- getNs
        writeIORef ansRef ans
        return (e - s)
        ) [1..iters]

    let sortedTimes = sort times
        medianNs = sortedTimes !! (iters `div` 2)
    answer <- readIORef ansRef

    putStrLn $ "BENCHMARK|problem=" ++ pad3 problem
            ++ "|answer=" ++ show answer
            ++ "|time_ns=" ++ show medianNs
            ++ "|iterations=" ++ show iters

-- | Zero-pad an integer to 3 digits
pad3 :: Int -> String
pad3 n
    | n < 10    = "00" ++ show n
    | n < 100   = "0" ++ show n
    | otherwise = show n
