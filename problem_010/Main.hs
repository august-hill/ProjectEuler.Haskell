-- Problem 010: Summation of Primes
-- Find the sum of all primes below two million.
-- Answer: 142913828922

module Main where

import Bench (runBench)

-- Simple trial division sieve
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes

solve :: Integer
solve = sum $ takeWhile (< 2000000) primes

main :: IO ()
main = runBench 10 (return solve)
