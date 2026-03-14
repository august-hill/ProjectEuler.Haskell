-- Problem 034: Digit Factorials
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

factorials :: [Int]
factorials = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

digitFactorialSum :: Int -> Int
digitFactorialSum = sum . map (\c -> factorials !! digitToInt c) . show

-- Upper bound: 7 * 9! = 2540160
solve :: Int
solve = sum [n | n <- [3..2540160], n == digitFactorialSum n]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
