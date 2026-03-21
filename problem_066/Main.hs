-- Problem 066: Diophantine Equation
-- Find D <= 1000 for which the minimal x in x^2 - D*y^2 = 1 is maximised.
-- Answer: 661

module Main where

import Bench (runBench)

-- Solve Pell's equation using continued fraction expansion
minimalX :: Integer -> Integer
minimalX d
    | s * s == d = 0
    | otherwise  = go 0 1 s [s] [1, s]
  where
    s = floor (sqrt (fromIntegral d :: Double))
    go m dn a hs ks
        | length hs > 2 && x * x - d * y * y == 1 = x
        | otherwise = go m' dn' a' (hs ++ [h']) (ks ++ [k'])
      where
        m' = dn * a - m
        dn' = (d - m' * m') `div` dn
        a' = (s + m') `div` dn'
        h' = a' * last hs + hs !! (length hs - 2)
        k' = a' * last ks + ks !! (length ks - 2)
        x = last hs
        y = last ks

solve :: Integer
solve = snd $ maximum [(minimalX d, d) | d <- [2..1000], let s = floor (sqrt (fromIntegral d :: Double)), s * s /= d]

main :: IO ()
main = runBench 66 (return solve)
