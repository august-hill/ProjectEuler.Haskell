-- Problem 074: Digit Factorial Chains
-- How many chains, with starting number below one million, have exactly 60 non-repeating terms?
-- Answer: 402

module Main where

import Bench (runBench)
import Data.Char (digitToInt)
import qualified Data.Set as Set

factorials :: [Int]
factorials = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

digitFactSum :: Int -> Int
digitFactSum = sum . map (\c -> factorials !! digitToInt c) . show

chainLength :: Int -> Int
chainLength n = go n Set.empty
  where
    go x seen
        | Set.member x seen = Set.size seen
        | otherwise = go (digitFactSum x) (Set.insert x seen)

solve :: Int
solve = length [n | n <- [1..999999], chainLength n == 60]

main :: IO ()
main = runBench 74 (return solve)
