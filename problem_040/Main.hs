-- Problem 040: Champernowne's Constant
-- Find d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
-- Answer: 210

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

champernowne :: String
champernowne = concatMap show [1..]

solve :: Int
solve = product [ digitToInt (champernowne !! (n - 1))
                | n <- [1, 10, 100, 1000, 10000, 100000, 1000000] ]

main :: IO ()
main = runBench 40 (return solve)
