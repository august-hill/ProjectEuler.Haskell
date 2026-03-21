-- Problem 075: Singular Integer Right Triangles
-- How many values of L <= 1,500,000 can form exactly one integer-sided right triangle?
-- Answer: 161667

module Main where

import Bench (runBench)
import qualified Data.Map.Strict as Map

limit :: Int
limit = 1500000

solve :: Int
solve = Map.size $ Map.filter (== 1) perimeterCounts
  where
    -- Generate primitive Pythagorean triples using Euclid's formula
    triples = [ (a, b, c)
              | m <- [2..floor (sqrt (fromIntegral limit / 2 :: Double))]
              , n <- [1..m-1]
              , odd (m - n)
              , gcd m n == 1
              , let a = m*m - n*n
              , let b = 2*m*n
              , let c = m*m + n*n
              , a + b + c <= limit
              ]
    perimeterCounts = Map.fromListWith (+)
        [ (k * p, 1 :: Int)
        | (a, b, c) <- triples
        , let p = a + b + c
        , k <- [1..limit `div` p]
        ]

main :: IO ()
main = runBench 75 (return solve)
