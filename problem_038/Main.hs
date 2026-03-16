-- Problem 038: Pandigital Multiples
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
-- concatenated product of an integer with (1,2,...,n) where n > 1?

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
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
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
