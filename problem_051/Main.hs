-- Problem 051: Prime Digit Replacements
-- Find smallest prime where replacing 3 digits gives 8 primes.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char (intToDigit, digitToInt)
import qualified Data.Set as Set

upperLimit :: Int
upperLimit = 999999

lowerLimit :: Int
lowerLimit = 100000

primeSet :: Set.Set Int
primeSet = Set.fromList $ sieve [2..upperLimit]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > upperLimit = p : xs
        | otherwise          = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime = (`Set.member` primeSet)

-- For a 6-digit prime, try all combinations of 3 positions with same digit
solve :: Int
solve = minimum [ fp
                | p <- [lowerLimit + 1..upperLimit]
                , isPrime p
                , let s = show p
                , let n = length s
                , i <- [0..n - 3]
                , j <- [i + 1..n - 2]
                , k <- [j + 1..n - 1]
                , s !! i == s !! j && s !! j == s !! k
                , let candidates = [ read (replaceAt i d (replaceAt j d (replaceAt k d s))) :: Int
                                   | d <- ['0'..'9']
                                   , let num = read (replaceAt i d (replaceAt j d (replaceAt k d s))) :: Int
                                   , num > lowerLimit
                                   , isPrime num
                                   ]
                , length candidates == 8
                , let fp = minimum candidates
                ]
  where
    replaceAt idx c xs = take idx xs ++ [c] ++ drop (idx + 1) xs

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
