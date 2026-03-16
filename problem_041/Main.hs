-- Problem 041: Pandigital Prime
-- Find the largest n-digit pandigital prime.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (sort, permutations)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n < 4     = True
    | even n    = False
    | n `mod` 3 == 0 = False
    | otherwise = go 5
  where
    go i
        | i * i > n          = True
        | n `mod` i == 0     = False
        | n `mod` (i + 2) == 0 = False
        | otherwise          = go (i + 6)

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\acc d -> acc * 10 + d) 0

-- 8 and 9 digit pandigitals have digit sum divisible by 3, so not prime
-- Check 7-digit pandigitals first
solve :: Int
solve = maximum
    [ n
    | perm <- permutations [1..7]
    , let n = digitsToNum perm
    , isPrime n
    ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
