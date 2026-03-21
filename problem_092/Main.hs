-- Problem 092: Square Digit Chains
-- How many starting numbers below 10 million arrive at 89?
-- Answer: 8581146

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

squareDigitSum :: Int -> Int
squareDigitSum = sum . map (\c -> let d = digitToInt c in d * d) . show

arrives89 :: Int -> Bool
arrives89 1 = False
arrives89 89 = True
arrives89 n = arrives89 (squareDigitSum n)

solve :: Int
solve = length [n | n <- [1..9999999], arrives89 n]

main :: IO ()
main = runBench 92 (return solve)
