-- Problem 056: Powerful Digit Sum
-- Find the maximum digital sum of a^b where a, b < 100.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

digitalSum :: Integer -> Int
digitalSum = sum . map digitToInt . show

solve :: Int
solve = maximum [ digitalSum (a ^ b) | a <- [2..99], b <- [2..99] ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
