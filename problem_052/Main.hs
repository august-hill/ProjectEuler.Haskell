-- Problem 052: Permuted Multiples
-- Find smallest x where x, 2x, 3x, 4x, 5x, 6x contain same digits.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (sort)

digitSig :: Int -> String
digitSig = sort . show

solve :: Int
solve = head [ x | x <- [1..], let sig = digitSig x
             , all (\m -> digitSig (x * m) == sig) [2..6] ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
