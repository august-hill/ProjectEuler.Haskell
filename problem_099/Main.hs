-- Problem 099: Largest Exponential
-- Find the line number with the greatest numerical value (base,exponent pairs).
-- Answer: 709

module Main where

import Bench (runBench)

parsePairs :: String -> [(Double, Double)]
parsePairs = map parseLine . lines
  where
    parseLine s = let (a, _:b) = break (== ',') s
                  in (read a, read b)

solve :: [(Double, Double)] -> Int
solve pairs = snd $ maximum [(e * log b, i) | (i, (b, e)) <- zip [1..] pairs]

main :: IO ()
main = runBench 99 $ do
    contents <- readFile "base_exp.txt"
    return (solve (parsePairs contents))
