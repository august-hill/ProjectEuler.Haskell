-- Problem 007: 10001st Prime
-- Find the 10001st prime number.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes

solve :: Integer
solve = primes !! 10000

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
