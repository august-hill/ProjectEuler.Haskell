-- Problem 030: Digit Fifth Powers
-- Find the sum of all numbers that can be written as the sum of fifth powers of their digits.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

digitFifthPowerSum :: Int -> Int
digitFifthPowerSum = sum . map ((^ (5 :: Int)) . digitToInt) . show

-- Upper bound: 6 * 9^5 = 354294
solve :: Int
solve = sum [n | n <- [2..354294], n == digitFifthPowerSum n]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
