-- Problem 053: Combinatoric Selections
-- Count C(n,r) > 1,000,000 for 1 <= n <= 100.
-- Answer: 4075

module Main where

import Bench (runBench)

-- Use Pascal's triangle, cap values at threshold+1 to avoid huge numbers
solve :: Int
solve = go 1 [1] 0
  where
    threshold = 1000000
    go n prev count
        | n > 100   = count
        | otherwise  = go (n + 1) row (count + length (filter (> threshold) row))
      where
        row = 1 : zipWith (\a b -> min (a + b) (threshold + 1)) prev (tail prev) ++ [1]

main :: IO ()
main = runBench 53 (return solve)
