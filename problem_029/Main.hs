-- Problem 029: Distinct Powers
-- How many distinct terms are in the sequence a^b for 2 <= a <= 100 and 2 <= b <= 100?
-- Answer: 9183

module Main where

import Bench (runBench)
import qualified Data.Set as Set

solve :: Int
solve = Set.size $ Set.fromList [a^b | a <- [2..100] :: [Integer], b <- [2..100] :: [Int]]

main :: IO ()
main = runBench 29 (return solve)
