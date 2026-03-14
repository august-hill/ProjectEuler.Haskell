-- Problem 021: Amicable Numbers
-- Evaluate the sum of all amicable numbers under 10000.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

sumProperDivisors :: Int -> Int
sumProperDivisors n
    | n <= 1    = 0
    | otherwise = 1 + sum [i + n `div` i | i <- [2..sqrtN], n `mod` i == 0, i /= n `div` i]
                    + sum [i | i <- [2..sqrtN], n `mod` i == 0, i == n `div` i]
  where sqrtN = floor . sqrt $ fromIntegral n

solve :: Int
solve = sum [a | a <- [2..9999],
             let b = sumProperDivisors a,
             b /= a, b > 0, b < 10000,
             sumProperDivisors b == a]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
