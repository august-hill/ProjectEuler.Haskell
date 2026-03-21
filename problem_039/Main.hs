-- Problem 039: Integer Right Triangles
-- For which value of p <= 1000, is the number of solutions maximised?
-- Answer: 840

module Main where

import Bench (runBench)
import Data.List (maximumBy)
import Data.Ord (comparing)

solve :: Int
solve = fst $ maximumBy (comparing snd)
    [ (p, count p) | p <- [1..1000] ]
  where
    count p = length
        [ ()
        | a <- [1..p `div` 3]
        , b <- [a..(p - a) `div` 2]
        , let c = p - a - b
        , a * a + b * b == c * c
        ]

main :: IO ()
main = runBench 39 (return solve)
