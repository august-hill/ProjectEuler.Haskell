-- Problem 032: Pandigital Products
-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.
-- Answer: 45228

module Main where

import Bench (runBench)
import Data.List (sort, nub)

isPandigital :: Int -> Int -> Int -> Bool
isPandigital a b c = sort digits == "123456789"
  where digits = show a ++ show b ++ show c

solve :: Int
solve = sum $ nub [a * b | a <- [1..99],
                           let start = if a < 10 then 1000 else 100,
                           let end_  = if a < 10 then 9999 else 999,
                           b <- [start..end_],
                           isPandigital a b (a * b)]

main :: IO ()
main = runBench 32 (return solve)
