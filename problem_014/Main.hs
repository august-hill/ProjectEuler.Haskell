-- Problem 014: Longest Collatz Sequence
-- Find the starting number under one million that produces the longest chain.
-- Answer: 837799

module Main where

import Bench (runBench)

collatzLength :: Integer -> Int
collatzLength 1 = 1
collatzLength n
  | even n    = 1 + collatzLength (n `div` 2)
  | otherwise = 1 + collatzLength (3 * n + 1)

solve :: Integer
solve = snd $ maximum [(collatzLength n, n) | n <- [1..999999]]

main :: IO ()
main = runBench 14 (return solve)
