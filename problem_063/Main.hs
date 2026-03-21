-- Problem 063: Powerful Digit Counts
-- How many n-digit positive integers exist which are also an nth power?
-- Answer: 49

module Main where

import Bench (runBench)

solve :: Int
solve = length [ ()
               | n <- [1..30] :: [Int]
               , b <- [1..9] :: [Integer]
               , length (show (b ^ n)) == n
               ]

main :: IO ()
main = runBench 63 (return solve)
