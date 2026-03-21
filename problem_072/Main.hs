-- Problem 072: Counting Fractions
-- How many proper fractions n/d with d <= 1,000,000?
-- Answer: 303963552391

module Main where

import Bench (runBench)

-- Euler's totient using prime factorization
phi :: Int -> Int
phi 1 = 1
phi n = go n n 2
  where
    go result m p
        | p * p > m = if m > 1 then result - result `div` m else result
        | m `mod` p == 0 = go (result - result `div` p) (divOut m p) (p + 1)
        | otherwise = go result m (p + 1)
    divOut m p
        | m `mod` p == 0 = divOut (m `div` p) p
        | otherwise = m

solve :: Integer
solve = sum [fromIntegral (phi d) | d <- [2..1000000]]

main :: IO ()
main = runBench 72 (return solve)
