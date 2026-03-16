-- Problem 055: Lychrel Numbers
-- How many Lychrel numbers are there below ten-thousand?

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

reverseNum :: Integer -> Integer
reverseNum = read . reverse . show

isLychrel :: Integer -> Bool
isLychrel n = go (n + reverseNum n) 1
  where
    go _ 50     = True
    go x iter
        | x == reverseNum x = False
        | otherwise          = go (x + reverseNum x) (iter + 1)

solve :: Int
solve = length [ n | n <- [1..9999], isLychrel n ]

main :: IO ()
main = do
    start <- getCurrentTime
    let result = solve
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
