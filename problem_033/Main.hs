-- Problem 033: Digit Cancelling Fractions
-- Find the denominator of the product of the four "curious" fractions in lowest terms.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

solve :: Int
solve = denProduct `div` gcd numProduct denProduct
  where
    fractions = [(a * 10 + b, c * 10 + d) |
                 a <- [1..9], b <- [1..9], c <- [1..9], d <- [1..9],
                 let num = a * 10 + b,
                 let den = c * 10 + d,
                 num < den,
                 b == c,
                 a * den == num * d]
    numProduct = product $ map fst fractions
    denProduct = product $ map snd fractions

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
