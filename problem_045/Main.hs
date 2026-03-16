-- Problem 045: Triangular, Pentagonal, and Hexagonal
-- Find next number after 40755 that is triangular, pentagonal, and hexagonal.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

solve :: Integer
solve = go 2 2
  where
    go p h
        | pn == hn && hn > 40755 = hn
        | pn < hn    = go (p + 1) h
        | otherwise   = go p (h + 1)
      where
        pn = pentagonal p
        hn = hexagonal h

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
