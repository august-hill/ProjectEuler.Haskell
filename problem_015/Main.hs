-- Problem 015: Lattice Paths
-- How many routes are there through a 20x20 grid (only moving right and down)?

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- C(40, 20) = 40! / (20! * 20!)
solve :: Integer
solve = product [21..40] `div` product [1..20]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
