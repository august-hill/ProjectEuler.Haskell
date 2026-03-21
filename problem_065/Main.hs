-- Problem 065: Convergents of e
-- Find the sum of digits in the numerator of the 100th convergent of e.
-- Answer: 272

module Main where

import Bench (runBench)
import Data.Char (digitToInt)

-- e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ...]
eCF :: [Integer]
eCF = 2 : concatMap (\k -> [1, 2*k, 1]) [1..]

convergentNum :: Int -> Integer
convergentNum n = fst $ foldr (\a (h, k) -> (a * h + k, h)) (1, 0) (take n eCF)

solve :: Int
solve = sum $ map digitToInt $ show $ convergentNum 100

main :: IO ()
main = runBench 65 (return solve)
