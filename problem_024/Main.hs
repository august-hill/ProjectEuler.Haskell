-- Problem 024: Lexicographic Permutations
-- What is the millionth lexicographic permutation of the digits 0-9?
-- Answer: 2783915460

module Main where

import Bench (runBench)

factorial :: Int -> Int
factorial n = product [1..n]

-- Given a 0-indexed target, pick digits from the available list
nthPermutation :: Int -> [Int] -> [Int]
nthPermutation _ []     = []
nthPermutation n digits = d : nthPermutation r remaining
  where
    fact      = factorial (length digits - 1)
    (idx, r)  = n `divMod` fact
    d         = digits !! idx
    remaining = take idx digits ++ drop (idx + 1) digits

solve :: Integer
solve = read $ concatMap show $ nthPermutation 999999 [0..9]

main :: IO ()
main = runBench 24 (return solve)
