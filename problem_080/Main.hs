-- Problem 080: Square Root Digital Expansion
-- For the first 100 natural numbers, find the total of the digital sums of
-- the first 100 decimal digits of all irrational square roots.
-- Answer: 40886

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

-- Use Newton's method with large integers for precision
sqrtDigits :: Integer -> Int -> [Int]
sqrtDigits n digits = map digitToInt $ take digits $ show result
  where
    -- Compute floor(sqrt(n * 10^(2*digits)))
    target = n * 10 ^ (2 * digits)
    result = newtonSqrt target

newtonSqrt :: Integer -> Integer
newtonSqrt n = go (n `div` 2 + 1)
  where
    go x
        | x' >= x   = x
        | otherwise  = go x'
      where x' = (x + n `div` x) `div` 2

isSquare :: Int -> Bool
isSquare n = let s = floor (sqrt (fromIntegral n :: Double)) in s * s == n

solve :: Int
solve = sum [sum (sqrtDigits (fromIntegral n) 100) | n <- [2..99], not (isSquare n)]

main :: IO ()
main = runBench 80 (return solve)
