-- Problem 027: Quadratic Primes
-- Find the product of coefficients a and b for n^2 + an + b
-- that produces the maximum number of primes for consecutive values of n.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set

primeLimit :: Int
primeLimit = 1000000

-- Simple sieve of Eratosthenes
sieve :: Int -> Set.Set Int
sieve limit = Set.fromList $ go [2..limit]
  where
    go []     = []
    go (p:xs)
        | p * p > limit = p : xs
        | otherwise     = p : go [x | x <- xs, x `mod` p /= 0]

primeSet :: Set.Set Int
primeSet = sieve primeLimit

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = Set.member n primeSet

consecutivePrimes :: Int -> Int -> Int
consecutivePrimes a b = length $ takeWhile isPrime [n*n + a*n + b | n <- [0..]]

solve :: Int
solve = snd $ maximum [(consecutivePrimes a b, a * b) | a <- [-999..999], b <- [-1000..1000]]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
