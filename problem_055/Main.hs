-- Problem 055: Lychrel Numbers
-- How many Lychrel numbers are there below ten-thousand?
-- Answer: 249

module Main where

import Bench (runBench)

reverseNum :: Integer -> Integer
reverseNum = read . reverse . show

isLychrel :: Integer -> Bool
isLychrel n = go (n + reverseNum n) 1
  where
    go _ 50     = True
    go x iter
        | x == reverseNum x = False
        | otherwise          = go (x + reverseNum x) (iter + 1)

solve :: Int
solve = length [ n | n <- [1..9999], isLychrel n ]

main :: IO ()
main = runBench 55 (return solve)
