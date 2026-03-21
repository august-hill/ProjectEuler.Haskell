-- Problem 057: Square Root Convergents
-- How many fractions in the first 1000 expansions of sqrt(2) have more digits in numerator?
-- Answer: 153

module Main where

import Bench (runBench)

numDigits :: Integer -> Int
numDigits = length . show

-- sqrt(2) = 1 + 1/(2 + 1/(2 + ...))
-- h(n) = 2*h(n-1) + h(n-2), d(n) = 2*d(n-1) + d(n-2)
-- Starting: h(-1)=1, h(0)=1+1=3/2 => num=3, den=2
-- Actually: convergents of [1; 2,2,2,...]:
-- h_{-1}=1, h_0=1, d_{-1}=0, d_0=1
-- Then h_n = a_n * h_{n-1} + h_{n-2} where a_n = 2 for all n>=1
solve :: Int
solve = length $ filter id $ take 1000 $ zipWith (\n d -> numDigits n > numDigits d) nums dens
  where
    -- Generate convergents: 1 + 1/(2+1/(2+...))
    -- Starting h_{-1}=1, h_0=1, d_{-1}=0, d_0=1
    -- Then for expansion k: h_k = 2*h_{k-1} + h_{k-2}, d_k = 2*d_{k-1} + d_{k-2}
    -- The fraction is (h_{k-1} + h_k) / (d_{k-1} + d_k) = numerator/denominator
    -- Actually simpler: track n, d where n/d is the fraction 1+continued fraction
    -- n_1 = 3, d_1 = 2; n_{k+1} = n_k + 2*d_k, d_{k+1} = n_k + d_k
    fracs = iterate (\(n, d) -> (n + 2 * d, n + d)) (3 :: Integer, 2 :: Integer)
    nums = map fst fracs
    dens = map snd fracs

main :: IO ()
main = runBench 57 (return solve)
