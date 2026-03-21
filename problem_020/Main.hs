-- Problem 020: Factorial Digit Sum
-- Find the sum of the digits in 100!
-- Answer: 648

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

solve :: Int
solve = sum $ map digitToInt $ show (product [1..100] :: Integer)

main :: IO ()
main = runBench 20 (return solve)
