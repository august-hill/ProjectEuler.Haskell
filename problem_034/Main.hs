-- Problem 034: Digit Factorials
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.
-- Answer: 40730

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

factorials :: [Int]
factorials = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

digitFactorialSum :: Int -> Int
digitFactorialSum = sum . map (\c -> factorials !! digitToInt c) . show

-- Upper bound: 7 * 9! = 2540160
solve :: Int
solve = sum [n | n <- [3..2540160], n == digitFactorialSum n]

main :: IO ()
main = runBench 34 (return solve)
