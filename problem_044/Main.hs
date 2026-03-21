-- Problem 044: Pentagon Numbers
-- Find pair of pentagonal numbers where sum and difference are both pentagonal.
-- Answer: 5482660

module Main where

import Bench (runBench)
import qualified Data.Set as Set

pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `div` 2

pentSet :: Set.Set Int
pentSet = Set.fromList [ pentagonal n | n <- [1..3000] ]

isPentagonal :: Int -> Bool
isPentagonal = (`Set.member` pentSet)

solve :: Int
solve = head
    [ pk - pj
    | j <- [1..2999]
    , let pj = pentagonal j
    , k <- [j + 1..3000]
    , let pk = pentagonal k
    , isPentagonal (pk + pj)
    , isPentagonal (pk - pj)
    ]

main :: IO ()
main = runBench 44 (return solve)
