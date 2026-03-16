-- Problem 050: Consecutive Prime Sum
-- Find prime < 1,000,000 that is sum of most consecutive primes.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set

limit :: Int
limit = 1000000

primes :: [Int]
primes = sieve [2..limit]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > limit = p : xs
        | otherwise     = p : sieve [x | x <- xs, x `mod` p /= 0]

primeSet :: Set.Set Int
primeSet = Set.fromList primes

isPrime :: Int -> Bool
isPrime = (`Set.member` primeSet)

solve :: Int
solve = snd $ foldl check (0, 0) [0..length primes - 1]
  where
    check (maxLen, maxSum) i = go (i + 1) (primes !! i) maxLen maxSum
      where
        go j s ml ms
            | j >= length primes || s >= limit = (ml, ms)
            | isPrime s && (j - i) > ml = go (j + 1) (s + primes !! j) (j - i) s
            | otherwise = go (j + 1) (s + primes !! j) ml ms

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
