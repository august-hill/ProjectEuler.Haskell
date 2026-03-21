-- Problem 061: Cyclical Figurate Numbers
-- Find the sum of the only ordered set of six cyclic 4-digit figurate numbers.
-- Answer: 28684

module Main where

import Bench (runBench)
import Data.List (permutations, nub)

polygonal :: Int -> Int -> Int
polygonal 3 n = n * (n + 1) `div` 2
polygonal 4 n = n * n
polygonal 5 n = n * (3 * n - 1) `div` 2
polygonal 6 n = n * (2 * n - 1)
polygonal 7 n = n * (5 * n - 3) `div` 2
polygonal 8 n = n * (3 * n - 2)
polygonal _ _ = 0

fourDigit :: Int -> [Int]
fourDigit s = takeWhile (<= 9999) $ dropWhile (< 1000) [polygonal s n | n <- [1..]]

cycles :: Int -> Int -> Bool
cycles a b = a `mod` 100 == b `div` 100

findChain :: [[Int]] -> [Int] -> [[Int]]
findChain [] chain
    | cycles (last chain) (head chain) = [chain]
    | otherwise = []
findChain remaining chain =
    [ result
    | (i, nums) <- zip [0..] remaining
    , n <- nums
    , cycles (last chain) n
    , result <- findChain (take i remaining ++ drop (i+1) remaining) (chain ++ [n])
    ]

solve :: Int
solve = head
    [ sum chain
    | perm <- permutations [4, 5, 6, 7, 8]
    , let sets = map fourDigit (3 : perm)
    , n <- head sets
    , chain <- findChain (tail sets) [n]
    ]

main :: IO ()
main = runBench 61 (return solve)
