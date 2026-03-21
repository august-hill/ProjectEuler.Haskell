-- Problem 016: Power Digit Sum
-- Find the sum of the digits of 2^1000.
-- Answer: 1366

module Main where

import Data.Char (digitToInt)
import Bench (runBench)

solve :: Int
solve = sum . map digitToInt . show $ (2 ^ 1000 :: Integer)

main :: IO ()
main = runBench 16 (return solve)
