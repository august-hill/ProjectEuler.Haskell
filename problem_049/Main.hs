-- Problem 049: Prime Permutations
-- Find 4-digit arithmetic sequence of 3 primes that are permutations (not 1487).

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (sort)
import qualified Data.Set as Set

primeSet :: Set.Set Int
primeSet = Set.fromList $ sieve [2..9999]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > 9999 = p : xs
        | otherwise     = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime = (`Set.member` primeSet)

digitSig :: Int -> String
digitSig = sort . show

solve :: Integer
solve = head
    [ read (show p ++ show q ++ show r)
    | p <- [1000..9999]
    , p /= 1487
    , isPrime p
    , let sig = digitSig p
    , q <- [p + 1..9999]
    , isPrime q
    , digitSig q == sig
    , let r = q + (q - p)
    , r < 10000
    , isPrime r
    , digitSig r == sig
    ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
