-- Problem 010: Summation of Primes
-- Find the sum of all primes below two million.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Simple trial division sieve
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes

solve :: Integer
solve = sum $ takeWhile (< 2000000) primes

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
