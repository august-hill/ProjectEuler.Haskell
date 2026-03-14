-- Problem 020: Factorial Digit Sum
-- Find the sum of the digits in 100!

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

solve :: Int
solve = sum $ map digitToInt $ show (product [1..100] :: Integer)

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
