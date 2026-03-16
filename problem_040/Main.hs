-- Problem 040: Champernowne's Constant
-- Find d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

champernowne :: String
champernowne = concatMap show [1..]

solve :: Int
solve = product [ digitToInt (champernowne !! (n - 1))
                | n <- [1, 10, 100, 1000, 10000, 100000, 1000000] ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
