-- Problem 004: Largest Palindrome Product
-- Find the largest palindrome made from the product of two 3-digit numbers.
-- Answer: 906609

module Main where

import Bench (runBench)

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

solve :: Integer
solve = maximum [x * y | x <- [100..999], y <- [x..999], isPalindrome (x * y)]

main :: IO ()
main = runBench 4 (return solve)
