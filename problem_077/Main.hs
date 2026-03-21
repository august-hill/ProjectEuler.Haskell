-- Problem 077: Prime Summations
-- Find the first value which can be written as the sum of primes in over 5000 ways.
-- Answer: 71

module Main where

import Bench (runBench)
import Data.Array (Array, listArray, (!))

primes :: [Int]
primes = sieve [2..1000]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > 1000 = p : xs
        | otherwise    = p : sieve [x | x <- xs, x `mod` p /= 0]

countWays :: Int -> Int
countWays target = dp ! (length ps, target)
  where
    ps = takeWhile (<= target) primes
    np = length ps
    dp :: Array (Int, Int) Int
    dp = listArray ((0, 0), (np, target))
        [f i n | i <- [0..np], n <- [0..target]]
    f 0 0 = 1
    f 0 _ = 0
    f i n
        | p <= n    = dp ! (i, n - p) + dp ! (i - 1, n)
        | otherwise = dp ! (i - 1, n)
      where p = ps !! (i - 1)

solve :: Int
solve = head [n | n <- [2..], countWays n > 5000]

main :: IO ()
main = runBench 77 (return solve)
