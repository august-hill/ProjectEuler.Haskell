-- Problem 023: Non-abundant Sums
-- Find the sum of all positive integers which cannot be written as the sum of two abundant numbers.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set

sumProperDivisors :: Int -> Int
sumProperDivisors n
    | n <= 1    = 0
    | otherwise = 1 + sum divisorPairs
  where
    sqrtN = floor . sqrt $ fromIntegral n
    divisorPairs = concatMap pairFor [2..sqrtN]
    pairFor i
        | n `mod` i /= 0 = []
        | i == n `div` i  = [i]
        | otherwise        = [i, n `div` i]

isAbundant :: Int -> Bool
isAbundant n = sumProperDivisors n > n

limit :: Int
limit = 28123

solve :: Int
solve = sum [i | i <- [1..limit], not (Set.member i abundantSums)]
  where
    abundants = [n | n <- [12..limit], isAbundant n]
    abundantSums = Set.fromList [a + b | a <- abundants, b <- abundants,
                                         a <= b, a + b <= limit]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
