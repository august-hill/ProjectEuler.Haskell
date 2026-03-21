-- Problem 093: Arithmetic Expressions
-- Find the set of four digits which gives the longest set of consecutive positive integers.
-- Answer: 1258

module Main where

import Bench (runBench)
import Data.List (permutations, nub, sort)
import qualified Data.Set as Set

ops :: [Double -> Double -> [Double]]
ops = [\a b -> [a+b], \a b -> [a-b], \a b -> [a*b], \a b -> if b /= 0 then [a/b] else []]

allResults :: [Double] -> [Double]
allResults [x] = [x]
allResults xs = nub
    [ r
    | i <- [0..length xs - 1]
    , j <- [0..length xs - 1]
    , i /= j
    , op <- ops
    , v <- op (xs !! i) (xs !! j)
    , r <- allResults (v : [xs !! k | k <- [0..length xs - 1], k /= i, k /= j])
    ]

consecutiveCount :: Set.Set Int -> Int
consecutiveCount s = length $ takeWhile (`Set.member` s) [1..]

solve :: Int
solve = snd $ maximum
    [ (consecutiveCount resultSet, a * 1000 + b * 100 + c * 10 + d)
    | a <- [1..6], b <- [a+1..7], c <- [b+1..8], d <- [c+1..9]
    , let results = allResults (map fromIntegral [a, b, c, d])
    , let resultSet = Set.fromList [round r | r <- results, r > 0, abs (r - fromIntegral (round r)) < 1e-9]
    ]

main :: IO ()
main = runBench 93 (return solve)
