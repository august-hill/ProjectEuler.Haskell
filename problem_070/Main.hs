-- Problem 070: Totient Permutation
-- Find n < 10^7 where phi(n) is a permutation of n and n/phi(n) is minimised.
-- Answer: 8319823

module Main where

import Bench (runBench)
import Data.List (sort)

-- Sieve-based totient
totientSieve :: Int -> [Int]
totientSieve limit = go [0..limit] [2..limit]
  where
    go phi [] = phi
    go phi (p:ps)
        | phi !! p /= p = go phi ps  -- not prime
        | otherwise = go (foldl (update p) phi [p, 2*p..limit]) ps
    update p phi i = take i phi ++ [phi !! i - phi !! i `div` p] ++ drop (i+1) phi

-- More efficient: compute phi using prime factorization
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

isPermutation :: Int -> Int -> Bool
isPermutation a b = sort (show a) == sort (show b)

solve :: Int
solve = snd $ minimum [(fromIntegral n / fromIntegral (phi n) :: Double, n) | n <- [2..9999999], let p = phi n, isPermutation n p]

main :: IO ()
main = runBench 70 (return solve)
