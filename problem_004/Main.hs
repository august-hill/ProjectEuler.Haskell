-- Problem 004: Largest Palindrome Product
-- Find the largest palindrome made from the product of two 3-digit numbers.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in s == reverse s

solve :: Integer
solve = maximum [x * y | x <- [100..999], y <- [x..999], isPalindrome (x * y)]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
