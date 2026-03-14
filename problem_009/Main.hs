-- Problem 009: Special Pythagorean Triplet
-- Find the product abc where a + b + c = 1000 and a^2 + b^2 = c^2.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

solve :: Integer
solve = head [a * b * c | a <- [1..998],
                           b <- [a+1..998],
                           let c = 1000 - a - b,
                           c > b,
                           a*a + b*b == c*c]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
