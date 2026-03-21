-- Problem 071: Ordered Fractions
-- Find the numerator of the fraction immediately left of 3/7 with d <= 1,000,000.
-- Answer: 428570

module Main where

import Bench (runBench)

solve :: Int
solve = fst $ foldl update (0, 1) [2..1000000]
  where
    update (bestN, bestD) d =
        let n = (3 * d - 1) `div` 7
        in if n * bestD > bestN * d && (n, d) /= (3, 7)
           then (n, d)
           else (bestN, bestD)

main :: IO ()
main = runBench 71 (return solve)
