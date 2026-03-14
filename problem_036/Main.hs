-- Problem 036: Double-base Palindromes
-- Find the sum of all numbers < 1,000,000 which are palindromic in both base 10 and base 2.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

toBinary :: Int -> String
toBinary n = showIntAtBase 2 intToDigit n ""

solve :: Int
solve = sum [n | n <- [1..999999],
             isPalindrome (show n),
             isPalindrome (toBinary n)]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
