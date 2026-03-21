-- Problem 060: Prime Pair Sets
-- Find the lowest sum for a set of five primes where any two concatenate to a prime.
-- Answer: 26033

module Main where

import Bench (runBench)
import qualified Data.Set as Set

limit :: Int
limit = 10000

primes :: [Int]
primes = sieve [2..limit]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > limit = p : xs
        | otherwise     = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n < 4     = True
    | even n    = False
    | n `mod` 3 == 0 = False
    | otherwise = go 5
  where
    go i
        | i * i > n          = True
        | n `mod` i == 0     = False
        | n `mod` (i + 2) == 0 = False
        | otherwise          = go (i + 6)

concatNums :: Int -> Int -> Int
concatNums a b = read (show a ++ show b)

pairPrime :: Int -> Int -> Bool
pairPrime a b = isPrime (concatNums a b) && isPrime (concatNums b a)

solve :: Int
solve = minimum
    [ a + b + c + d + e
    | (ia, a) <- zip [0..] primes
    , (ib, b) <- zip [0..] primes, ib > ia, pairPrime a b
    , (ic, c) <- zip [0..] primes, ic > ib, pairPrime a c, pairPrime b c
    , (_, d) <- zip [0..] primes, d > c, pairPrime a d, pairPrime b d, pairPrime c d
    , e <- primes, e > d, pairPrime a e, pairPrime b e, pairPrime c e, pairPrime d e
    ]

main :: IO ()
main = runBench 60 (return solve)
