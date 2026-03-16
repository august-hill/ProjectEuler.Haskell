-- Problem 043: Sub-string Divisibility
-- Find sum of 0-9 pandigitals with substring divisibility.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (permutations)

digitsToNum :: [Int] -> Integer
digitsToNum = foldl (\acc d -> acc * 10 + fromIntegral d) 0

subNum :: [Int] -> Int -> Int
subNum ds i = ds !! i * 100 + ds !! (i + 1) * 10 + ds !! (i + 2)

hasProperty :: [Int] -> Bool
hasProperty ds = and $ zipWith (\i p -> subNum ds i `mod` p == 0) [1..7] [2,3,5,7,11,13,17]

solve :: Integer
solve = sum [ digitsToNum p | p <- permutations [0..9], hasProperty p ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
