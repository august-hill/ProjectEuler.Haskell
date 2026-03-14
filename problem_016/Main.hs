-- Problem 016: Power Digit Sum
-- Find the sum of the digits of 2^1000.

module Main where

import Data.Char (digitToInt)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

solve :: Int
solve = sum . map digitToInt . show $ (2 ^ 1000 :: Integer)

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
