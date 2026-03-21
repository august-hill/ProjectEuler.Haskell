-- Problem 036: Double-base Palindromes
-- Find the sum of all numbers < 1,000,000 which are palindromic in both base 10 and base 2.
-- Answer: 872187

module Main where

import Bench (runBench)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

toBinary :: Int -> String
toBinary n = showIntAtBase 2 intToDigit n ""

solve :: Int
solve = sum [n | n <- [1..999999],
             isPalindrome (show n),
             isPalindrome (toBinary n)]

main :: IO ()
main = runBench 36 (return solve)
