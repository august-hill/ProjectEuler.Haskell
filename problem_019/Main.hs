-- Problem 019: Counting Sundays
-- How many Sundays fell on the first of the month during the 20th century?
-- Answer: 171

module Main where

import Bench (runBench)

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

daysInMonth :: Int -> Int -> Int
daysInMonth 2 y
    | isLeapYear y = 29
    | otherwise    = 28
daysInMonth m _
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise               = 31

solve :: Int
solve = length $ filter (== 0) firstOfMonths
  where
    -- Jan 1, 1900 = Monday (1), advance through 1900 to get day of week for Jan 1, 1901
    startDay = foldl (\d m -> (d + daysInMonth m 1900) `mod` 7) 1 [1..12]
    -- Accumulate day-of-week for the 1st of each month from 1901-2000
    firstOfMonths = scanl advance startDay [(y, m) | y <- [1901..2000], m <- [1..12]]
    advance d (y, m) = (d + daysInMonth m y) `mod` 7

main :: IO ()
main = runBench 19 (return solve)
