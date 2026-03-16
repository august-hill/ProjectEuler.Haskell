-- Problem 048: Self Powers
-- Find last 10 digits of 1^1 + 2^2 + 3^3 + ... + 1000^1000.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

modulus :: Integer
modulus = 10000000000

modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 _ = 1
modPow base ex m
    | even ex   = modPow (base * base `mod` m) (ex `div` 2) m
    | otherwise = base * modPow base (ex - 1) m `mod` m

solve :: Integer
solve = sum [ modPow i i modulus | i <- [1..1000] ] `mod` modulus

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
