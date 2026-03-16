-- Problem 039: Integer Right Triangles
-- For which value of p <= 1000, is the number of solutions maximised?

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (maximumBy)
import Data.Ord (comparing)

solve :: Int
solve = fst $ maximumBy (comparing snd)
    [ (p, count p) | p <- [1..1000] ]
  where
    count p = length
        [ ()
        | a <- [1..p `div` 3]
        , b <- [a..(p - a) `div` 2]
        , let c = p - a - b
        , a * a + b * b == c * c
        ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
