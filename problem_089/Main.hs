-- Problem 089: Roman Numerals
-- Find the number of characters saved by writing Roman numerals in minimal form.
-- Answer: 743

module Main where

import Bench (runBench)

fromRoman :: String -> Int
fromRoman [] = 0
fromRoman [c] = romanVal c
fromRoman (c1:c2:rest)
    | romanVal c1 < romanVal c2 = romanVal c2 - romanVal c1 + fromRoman rest
    | otherwise = romanVal c1 + fromRoman (c2:rest)

romanVal :: Char -> Int
romanVal 'I' = 1
romanVal 'V' = 5
romanVal 'X' = 10
romanVal 'L' = 50
romanVal 'C' = 100
romanVal 'D' = 500
romanVal 'M' = 1000
romanVal _   = 0

toRoman :: Int -> String
toRoman 0 = ""
toRoman n
    | n >= 1000 = 'M' : toRoman (n - 1000)
    | n >= 900  = "CM" ++ toRoman (n - 900)
    | n >= 500  = 'D' : toRoman (n - 500)
    | n >= 400  = "CD" ++ toRoman (n - 400)
    | n >= 100  = 'C' : toRoman (n - 100)
    | n >= 90   = "XC" ++ toRoman (n - 90)
    | n >= 50   = 'L' : toRoman (n - 50)
    | n >= 40   = "XL" ++ toRoman (n - 40)
    | n >= 10   = 'X' : toRoman (n - 10)
    | n >= 9    = "IX" ++ toRoman (n - 9)
    | n >= 5    = 'V' : toRoman (n - 5)
    | n >= 4    = "IV" ++ toRoman (n - 4)
    | n >= 1    = 'I' : toRoman (n - 1)
    | otherwise = ""

solve :: [String] -> Int
solve numerals = sum [length old - length (toRoman (fromRoman old)) | old <- numerals]

main :: IO ()
main = runBench 89 $ do
    contents <- readFile "roman.txt"
    return (solve (lines contents))
