-- Problem 003: Largest Prime Factor
-- Find the largest prime factor of 600851475143.
-- Answer: 6857

module Main where

import Bench (runBench)

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = go n 2
  where
    go 1 _ = 1
    go m d
      | d * d > m = m
      | m `mod` d == 0 = go (divAll m d) (d + 1)
      | otherwise = go m (d + 1)
    divAll m d
      | m `mod` d == 0 = divAll (m `div` d) d
      | otherwise = m

solve :: Integer
solve = largestPrimeFactor 600851475143

main :: IO ()
main = runBench 3 (return solve)
