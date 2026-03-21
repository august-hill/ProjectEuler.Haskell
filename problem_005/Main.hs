-- Problem 005: Smallest Multiple
-- Find the smallest positive number evenly divisible by all numbers from 1 to 20.
-- Answer: 232792560

module Main where

import Bench (runBench)

solve :: Integer
solve = foldl1 lcm [1..20]

main :: IO ()
main = runBench 5 (return solve)
