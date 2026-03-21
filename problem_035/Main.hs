-- Problem 035: Circular Primes
-- How many circular primes are there below one million?
-- Answer: 55

module Main where

import Bench (runBench)
import qualified Data.Set as Set

limit :: Int
limit = 1000000

-- Sieve of Eratosthenes
primeSet :: Set.Set Int
primeSet = Set.fromList $ sieve [2..limit - 1]
  where
    sieve []     = []
    sieve (p:xs)
        | p * p >= limit = p : xs
        | otherwise      = p : sieve [x | x <- xs, x `mod` p /= 0]

rotations :: Int -> [Int]
rotations n = take len $ iterate rotate n
  where
    s      = show n
    len    = length s
    rotate x = let (d, r) = x `divMod` (10 ^ (len - 1))
               in r * 10 + d

isCircularPrime :: Int -> Bool
isCircularPrime n = all (`Set.member` primeSet) (rotations n)

solve :: Int
solve = length $ filter isCircularPrime (Set.toList primeSet)

main :: IO ()
main = runBench 35 (return solve)
