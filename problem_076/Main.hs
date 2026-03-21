-- Problem 076: Counting Summations
-- How many different ways can 100 be written as a sum of at least two positive integers?
-- Answer: 190569291

module Main where

import Bench (runBench)
import Data.Array (Array, listArray, (!))

solve :: Int
solve = ways ! (99, 100) -- ways to write 100 using parts 1..99
  where
    ways :: Array (Int, Int) Int
    ways = listArray ((0, 0), (99, 100))
        [f i n | i <- [0..99], n <- [0..100]]
    f 0 0 = 1
    f 0 _ = 0
    f i n
        | i <= n    = ways ! (i, n - i) + ways ! (i - 1, n)
        | otherwise = ways ! (i - 1, n)

main :: IO ()
main = runBench 76 (return solve)
