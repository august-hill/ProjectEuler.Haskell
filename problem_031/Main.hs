-- Problem 031: Coin Sums
-- How many different ways can 200p be made using coins?
-- Coins: 1p, 2p, 5p, 10p, 20p, 50p, 100p, 200p
-- Answer: 73682

module Main where

import Bench (runBench)
import Data.Array (Array, listArray, (!))

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

solve :: Int
solve = ways ! (length coins, 200)
  where
    ways :: Array (Int, Int) Int
    ways = listArray ((0, 0), (length coins, 200))
        [f i a | i <- [0..length coins], a <- [0..200]]
    f 0 0 = 1
    f 0 _ = 0
    f i a
        | coin <= a = ways ! (i, a - coin) + ways ! (i - 1, a)
        | otherwise = ways ! (i - 1, a)
      where coin = coins !! (i - 1)

main :: IO ()
main = runBench 31 (return solve)
