-- Problem 052: Permuted Multiples
-- Find smallest x where x, 2x, 3x, 4x, 5x, 6x contain same digits.
-- Answer: 142857

module Main where

import Bench (runBench)
import Data.List (sort)

digitSig :: Int -> String
digitSig = sort . show

solve :: Int
solve = head [ x | x <- [1..], let sig = digitSig x
             , all (\m -> digitSig (x * m) == sig) [2..6] ]

main :: IO ()
main = runBench 52 (return solve)
