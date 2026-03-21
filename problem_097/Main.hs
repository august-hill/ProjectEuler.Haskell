-- Problem 097: Large Non-Mersenne Prime
-- Find the last ten digits of 28433 * 2^7830457 + 1.
-- Answer: 8739992577

module Main where

import Bench (runBench)

modulus :: Integer
modulus = 10000000000

modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 _ = 1
modPow base ex m
    | even ex   = modPow ((base * base) `mod` m) (ex `div` 2) m
    | otherwise = (base * modPow base (ex - 1) m) `mod` m

solve :: Integer
solve = (28433 * modPow 2 7830457 modulus + 1) `mod` modulus

main :: IO ()
main = runBench 97 (return solve)
