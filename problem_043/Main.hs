-- Problem 043: Sub-string Divisibility
-- Find sum of 0-9 pandigitals with substring divisibility.
-- Answer: 16695334890

module Main where

import Bench (runBench)
import Data.List (permutations)

digitsToNum :: [Int] -> Integer
digitsToNum = foldl (\acc d -> acc * 10 + fromIntegral d) 0

subNum :: [Int] -> Int -> Int
subNum ds i = ds !! i * 100 + ds !! (i + 1) * 10 + ds !! (i + 2)

hasProperty :: [Int] -> Bool
hasProperty ds = and $ zipWith (\i p -> subNum ds i `mod` p == 0) [1..7] [2,3,5,7,11,13,17]

solve :: Integer
solve = sum [ digitsToNum p | p <- permutations [0..9], hasProperty p ]

main :: IO ()
main = runBench 43 (return solve)
