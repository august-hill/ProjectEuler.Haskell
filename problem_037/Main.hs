-- Problem 037: Truncatable Primes
-- Find the sum of the only eleven primes that are both truncatable from left to right
-- and right to left.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set

limit :: Int
limit = 1000000

primeSet :: Set.Set Int
primeSet = Set.fromList $ sieve [2..limit - 1]
  where
    sieve []     = []
    sieve (p:xs)
        | p * p >= limit = p : xs
        | otherwise      = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime n = n >= 2 && Set.member n primeSet

-- Remove digits from right: 3797 -> 379 -> 37 -> 3
rightTruncations :: Int -> [Int]
rightTruncations n
    | n < 10    = []
    | otherwise = let t = n `div` 10 in t : rightTruncations t

-- Remove digits from left: 3797 -> 797 -> 97 -> 7
leftTruncations :: Int -> [Int]
leftTruncations n = go n (numDigits n)
  where
    numDigits x
        | x < 10    = 1
        | otherwise = 1 + numDigits (x `div` 10)
    go _ 1 = []
    go x d = let t = x `mod` (10 ^ (d - 1)) in t : go t (d - 1)

isTruncatable :: Int -> Bool
isTruncatable n = all isPrime (rightTruncations n) && all isPrime (leftTruncations n)

solve :: Int
solve = sum $ take 11 [p | p <- [11..], isPrime p, isTruncatable p]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
