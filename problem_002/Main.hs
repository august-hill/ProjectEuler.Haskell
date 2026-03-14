-- Problem 002: Even Fibonacci Numbers
-- Find the sum of even Fibonacci numbers not exceeding four million.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

solve :: Integer
solve = sum . filter even . takeWhile (<= 4000000) $ fibs

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
