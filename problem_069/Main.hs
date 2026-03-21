-- Problem 069: Totient Maximum
-- Find n <= 1,000,000 for which n/phi(n) is a maximum.
-- Answer: 510510

module Main where

import Bench (runBench)

-- n/phi(n) is maximised for the product of the smallest primes
primes :: [Int]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23]

solve :: Int
solve = last $ takeWhile (<= 1000000) $ scanl1 (*) primes

main :: IO ()
main = runBench 69 (return solve)
