-- Problem 022: Names Scores
-- Total of all name scores in the file.

module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (sort)
import Data.Char (ord)

parseName :: String -> String
parseName = filter (/= '"')

parseNames :: String -> [String]
parseNames [] = []
parseNames s  = let (name, rest) = break (== ',') s
                in parseName name : case rest of
                    []      -> []
                    (_:cs)  -> parseNames cs

nameValue :: String -> Int
nameValue = sum . map (\c -> ord c - ord 'A' + 1)

solve :: [String] -> Int
solve names = sum $ zipWith (*) [1..] (map nameValue (sort names))

main :: IO ()
main = do
    start <- getCurrentTime
    contents <- readFile "names.txt"
    let names = parseNames contents
        result = solve names
    result `seq` putStrLn $ "Result: " ++ show result
    end <- getCurrentTime
    putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
