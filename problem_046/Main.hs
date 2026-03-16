-- Problem 046: Goldbach's Other Conjecture
-- Find smallest odd composite that cannot be written as prime + 2*square.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set

limit :: Int
limit = 10000

primeSet :: Set.Set Int
primeSet = Set.fromList $ sieve [2..limit]
  where
    sieve [] = []
    sieve (p:xs)
        | p * p > limit = p : xs
        | otherwise     = p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Int -> Bool
isPrime n = Set.member n primeSet

canBeWritten :: Int -> Bool
canBeWritten n = any (\y -> isPrime (n - 2 * y * y)) [1..sqrtLim]
  where sqrtLim = floor (sqrt (fromIntegral n / 2 :: Double))

solve :: Int
solve = head [ c | c <- [9,11..limit], not (isPrime c), not (canBeWritten c) ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
