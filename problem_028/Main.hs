-- Problem 028: Number Spiral Diagonals
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral?
-- Answer: 669171001

module Main where

import Bench (runBench)

-- Sum of four corners at layer n (odd): 4n^2 - 6(n-1)
solve :: Integer
solve = 1 + sum [4*n*n - 6*(n-1) | n <- [3,5..1001]]

main :: IO ()
main = runBench 28 (return solve)
