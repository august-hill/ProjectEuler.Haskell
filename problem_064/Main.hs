-- Problem 064: Odd Period Square Roots
-- How many continued fractions for sqrt(N), N <= 10000, have an odd period?
-- Answer: 1322

module Main where

import Bench (runBench)

cfPeriod :: Int -> Int
cfPeriod n
    | s * s == n = 0
    | otherwise  = go 0 1 s
  where
    s = floor (sqrt (fromIntegral n :: Double))
    go _ _ a
        | a == 2 * s = 1
    go m d a = let m' = d * a - m
                   d' = (n - m' * m') `div` d
                   a' = (s + m') `div` d'
               in 1 + go m' d' a'

solve :: Int
solve = length [n | n <- [2..10000], odd (cfPeriod n)]

main :: IO ()
main = runBench 64 (return solve)
