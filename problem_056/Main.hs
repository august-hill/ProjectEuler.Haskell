-- Problem 056: Powerful Digit Sum
-- Find the maximum digital sum of a^b where a, b < 100.
-- Answer: 972

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

digitalSum :: Integer -> Int
digitalSum = sum . map digitToInt . show

solve :: Int
solve = maximum [ digitalSum (a ^ b) | a <- [2..99], b <- [2..99] ]

main :: IO ()
main = runBench 56 (return solve)
