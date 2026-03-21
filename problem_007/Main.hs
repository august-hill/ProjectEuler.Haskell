-- Problem 007: 10001st Prime
-- Find the 10001st prime number.
-- Answer: 104743

module Main where

import Bench (runBench)

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes

solve :: Integer
solve = primes !! 10000

main :: IO ()
main = runBench 7 (return solve)
