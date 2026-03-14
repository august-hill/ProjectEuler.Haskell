-- Problem 006: Sum Square Difference
-- Find the difference between the sum of squares and the square of the sum
-- for the first 100 natural numbers.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

solve :: Integer
solve = squareOfSum - sumOfSquares
  where
    squareOfSum  = let s = sum [1..100] in s * s
    sumOfSquares = sum [x * x | x <- [1..100]]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
