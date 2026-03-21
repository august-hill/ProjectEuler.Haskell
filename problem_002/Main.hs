-- Problem 002: Even Fibonacci Numbers
-- Find the sum of even Fibonacci numbers not exceeding four million.
-- Answer: 4613732

module Main where

import Bench (runBench)

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

solve :: Integer
solve = sum . filter even . takeWhile (<= 4000000) $ fibs

main :: IO ()
main = runBench 2 (return solve)
