-- Problem 078: Coin Partitions
-- Find the least value of n for which p(n) is divisible by one million.
-- Answer: 55374

module Main where

import Bench (runBench)
import Data.Array (Array, listArray, (!))

-- Partition function using Euler's pentagonal number theorem
-- p(n) = sum_{k != 0} (-1)^{k+1} * p(n - k*(3k-1)/2)
solve :: Int
solve = head [n | n <- [1..], partitions ! n == 0]
  where
    maxN = 100000
    partitions :: Array Int Int
    partitions = listArray (0, maxN) [p n | n <- [0..maxN]]
    p 0 = 1
    p n = sum (takeWhile (/= 0) [term n k | k <- concatMap (\i -> [i, -i]) [1..]]) `mod` 1000000
    term n k
        | n - pent < 0 = 0
        | otherwise     = sign * (partitions ! (n - pent))
      where
        pent = k * (3 * k - 1) `div` 2
        sign = if odd k then 1 else -1

main :: IO ()
main = runBench 78 (return solve)
