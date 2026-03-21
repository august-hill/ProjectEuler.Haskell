-- Problem 026: Reciprocal Cycles
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle.
-- Answer: 983

module Main where

import Bench (runBench)
import qualified Data.Map.Strict as Map

cycleLength :: Int -> Int
cycleLength d = go 1 0 Map.empty
  where
    go 0 _ _ = 0
    go remainder pos seen =
        case Map.lookup remainder seen of
            Just prevPos -> pos - prevPos
            Nothing      -> go ((remainder * 10) `mod` d) (pos + 1) (Map.insert remainder pos seen)

solve :: Int
solve = snd $ maximum [(cycleLength d, d) | d <- [2..999]]

main :: IO ()
main = runBench 26 (return solve)
