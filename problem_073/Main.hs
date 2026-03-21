-- Problem 073: Counting Fractions in a Range
-- How many fractions lie between 1/3 and 1/2 for d <= 12,000?
-- Answer: 7295372

module Main where

import Bench (runBench)

solve :: Int
solve = length [ ()
               | d <- [2..12000]
               , n <- [d `div` 3 + 1 .. (d - 1) `div` 2]
               , gcd n d == 1
               ]

main :: IO ()
main = runBench 73 (return solve)
