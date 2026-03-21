-- Problem 100: Arranged Probability
-- Find the number of blue discs for the first arrangement with > 10^12 total discs
-- where P(two blue) = 1/2.
-- Answer: 756872327473

module Main where

import Bench (runBench)

-- b(b-1) / (n(n-1)) = 1/2
-- 2b(b-1) = n(n-1)
-- This is a Pell-like equation. Solutions follow the recurrence:
-- b_{k+1} = 3*b_k + 2*n_k - 2
-- n_{k+1} = 4*b_k + 3*n_k - 3
-- Starting with b=3, n=4 or b=15, n=21

solve :: Integer
solve = go 15 21
  where
    go b n
        | n > 1000000000000 = b
        | otherwise = go (3*b + 2*n - 2) (4*b + 3*n - 3)

main :: IO ()
main = runBench 100 (return solve)
