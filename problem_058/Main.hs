-- Problem 058: Spiral Primes
-- Side length where ratio of primes on diagonals falls below 10%.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n < 4     = True
    | even n    = False
    | n `mod` 3 == 0 = False
    | otherwise = go 5
  where
    go i
        | i * i > n          = True
        | n `mod` i == 0     = False
        | n `mod` (i + 2) == 0 = False
        | otherwise          = go (i + 6)

-- Corners of side length s: s^2, s^2-s+1, s^2-2s+2, s^2-3s+3
-- Only odd side lengths: s = 3, 5, 7, ...
solve :: Int
solve = go 3 0 1  -- starting: side=3, primes=0, total diag count=1 (center)
  where
    go s primes total
        | s > 3 && 10 * primes' < total' = s
        | otherwise = go (s + 2) primes' total'
      where
        corners = [ s*s, s*s - s + 1, s*s - 2*s + 2, s*s - 3*s + 3 ]
        newPrimes = length $ filter isPrime corners
        primes' = primes + newPrimes
        total' = total + 4

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
