-- Problem 022: Names Scores
-- Total of all name scores in the file.
-- Answer: 871198282

module Main where

import Bench (runBench)
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
main = runBench 22 $ do
    contents <- readFile "names.txt"
    let names = parseNames contents
    return (solve names)
