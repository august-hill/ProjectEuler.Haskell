-- Problem 015: Lattice Paths
-- How many routes are there through a 20x20 grid (only moving right and down)?
-- Answer: 137846528640

module Main where

import Bench (runBench)

-- C(40, 20) = 40! / (20! * 20!)
solve :: Integer
solve = product [21..40] `div` product [1..20]

main :: IO ()
main = runBench 15 (return solve)
