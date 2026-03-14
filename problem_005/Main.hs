-- Problem 005: Smallest Multiple
-- Find the smallest positive number evenly divisible by all numbers from 1 to 20.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

solve :: Integer
solve = foldl1 lcm [1..20]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
