-- Problem 038: Pandigital Multiples
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
-- concatenated product of an integer with (1,2,...,n) where n > 1?
-- Answer: 932718654

module Main where

import Bench (runBench)
import Data.List (sort)

isPandigital :: String -> Bool
isPandigital s = length s == 9 && sort s == "123456789"

concatProduct :: Int -> String
concatProduct num = go 1 ""
  where
    go n acc
        | length acc >= 9 = acc
        | otherwise       = go (n + 1) (acc ++ show (num * n))

solve :: Integer
solve = fromIntegral $ maximum
    [ read cp :: Int
    | num <- [1..9999]
    , let cp = concatProduct num
    , length cp == 9
    , isPandigital cp
    ]

main :: IO ()
main = runBench 38 (return solve)
