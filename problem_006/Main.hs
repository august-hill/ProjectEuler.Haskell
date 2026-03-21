-- Problem 006: Sum Square Difference
-- Find the difference between the sum of squares and the square of the sum
-- for the first 100 natural numbers.
-- Answer: 25164150

module Main where

import Bench (runBench)

solve :: Integer
solve = squareOfSum - sumOfSquares
  where
    squareOfSum  = let s = sum [1..100] in s * s
    sumOfSquares = sum [x * x | x <- [1..100]]

main :: IO ()
main = runBench 6 (return solve)
