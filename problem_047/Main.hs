-- Problem 047: Distinct Prime Factors
-- Find first of 4 consecutive integers each with 4 distinct prime factors.
-- Answer: 134043

module Main where

import Bench (runBench)

countDistinctPrimeFactors :: Int -> Int
countDistinctPrimeFactors n = go n 2 0
  where
    go 1 _ c = c
    go m p c
        | p * p > m = c + 1
        | m `mod` p == 0 = go (divOut m p) (p + 1) (c + 1)
        | otherwise = go m (p + 1) c
    divOut m p
        | m `mod` p == 0 = divOut (m `div` p) p
        | otherwise      = m

solve :: Int
solve = go 2 0
  where
    go i count
        | count == 4 = i - 4
        | countDistinctPrimeFactors i == 4 = go (i + 1) (count + 1)
        | otherwise = go (i + 1) 0

main :: IO ()
main = runBench 47 (return solve)
