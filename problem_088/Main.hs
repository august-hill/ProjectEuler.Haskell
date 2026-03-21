-- Problem 088: Product-sum Numbers
-- Find the sum of all minimal product-sum numbers for 2 <= k <= 12000.
-- Answer: 7587457

module Main where

import Bench (runBench)
import qualified Data.Map.Strict as Map
import Data.List (nub)

limit :: Int
limit = 12000

-- For each factorization of n, compute k = n - sum_of_factors + count_of_factors
-- because we pad with 1s: product stays n, sum = sum_factors + (k - count) * 1 = n
-- so k = n - sum_factors + count_factors

solve :: Int
solve = sum $ nub $ Map.elems minK
  where
    minK = foldl updateMap Map.empty [(k, n) | n <- [2..2*limit], (s, c) <- factorizations n n 2, let k = n - s + c, k >= 2, k <= limit]
    updateMap m (k, n) = Map.insertWith min k n m

factorizations :: Int -> Int -> Int -> [(Int, Int)]
factorizations _ 1 _ = [(0, 0)]
factorizations orig n minF =
    (n, 1) :  -- n itself as a single factor
    [ (f + s, 1 + c)
    | f <- [minF..floor (sqrt (fromIntegral n :: Double))]
    , n `mod` f == 0
    , (s, c) <- factorizations orig (n `div` f) f
    ]

main :: IO ()
main = runBench 88 (return solve)
