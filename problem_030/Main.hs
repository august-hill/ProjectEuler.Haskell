-- Problem 030: Digit Fifth Powers
-- Find the sum of all numbers that can be written as the sum of fifth powers of their digits.
-- Answer: 443839

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

digitFifthPowerSum :: Int -> Int
digitFifthPowerSum = sum . map ((^ (5 :: Int)) . digitToInt) . show

-- Upper bound: 6 * 9^5 = 354294
solve :: Int
solve = sum [n | n <- [2..354294], n == digitFifthPowerSum n]

main :: IO ()
main = runBench 30 (return solve)
