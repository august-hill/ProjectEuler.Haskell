-- Problem 025: 1000-digit Fibonacci Number
-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
-- Answer: 4782

module Main where

import Bench (runBench)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

solve :: Int
solve = fst $ head $ dropWhile (\(_, f) -> length (show f) < 1000) $ zip [1..] fibs

main :: IO ()
main = runBench 25 (return solve)
