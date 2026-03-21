-- Problem 090: Cube Digit Pairs
-- How many distinct arrangements of two cubes allow all square numbers to be displayed?
-- Answer: 1217

module Main where

import Bench (runBench)
import Data.List (subsequences, sort)

-- The squares to display: 01, 04, 09, 16, 25, 36, 49, 64, 81
-- 6 and 9 are interchangeable
squares :: [(Int, Int)]
squares = [(0,1),(0,4),(0,9),(1,6),(2,5),(3,6),(4,9),(6,4),(8,1)]

choose6 :: [Int] -> [[Int]]
choose6 ds = [s | s <- subsequences ds, length s == 6]

allDice :: [[Int]]
allDice = choose6 [0..9]

hasDigit :: [Int] -> Int -> Bool
hasDigit die d
    | d == 6 || d == 9 = 6 `elem` die || 9 `elem` die
    | otherwise = d `elem` die

canDisplay :: [Int] -> [Int] -> Bool
canDisplay d1 d2 = all (\(a, b) -> (hasDigit d1 a && hasDigit d2 b) || (hasDigit d1 b && hasDigit d2 a)) squares

solve :: Int
solve = length [(d1, d2) | (i, d1) <- zip [0..] allDice, (j, d2) <- zip [0..] allDice, j > i, canDisplay d1 d2]

main :: IO ()
main = runBench 90 (return solve)
