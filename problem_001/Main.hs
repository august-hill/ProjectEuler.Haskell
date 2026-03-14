-- Problem 001: Multiples of 3 and 5
-- Find the sum of all multiples of 3 or 5 below 1000.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

solve :: Integer
solve = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
