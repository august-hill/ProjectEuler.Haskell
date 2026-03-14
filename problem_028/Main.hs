-- Problem 028: Number Spiral Diagonals
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral?

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Sum of four corners at layer n (odd): 4n^2 - 6(n-1)
solve :: Integer
solve = 1 + sum [4*n*n - 6*(n-1) | n <- [3,5..1001]]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
