-- Problem 059: XOR Decryption
-- Decrypt the message using XOR with a 3-character lowercase key.
-- Answer: 129448

module Main where

import Bench (runBench)
import Data.Char (chr, ord, isAlpha, isSpace, isPunctuation, isDigit)
import Data.Bits (xor)

cipher :: [Int]
cipher = [79,59,12,2,79,35,8,28,20,2,3,68,8,9,68,45,0,12,9,67,68,35,26,24,37,7,8,68,10,36,20,27,3,11,0,58,44,26,13,68,29,30,31,68,54,45,26,27,4,54,67,8,68,34,30,31,68,46,25,13,68,29,45,31,12,4,35,8,46,67,6,68,12,0,35,54,68,25,49,45,8,68,7,8,54,19,2,68,21,45,13,68,8,0,12,2,68,14,6,0,63,68,25,37,7,8,68,10,54,19,2,68,6,29,45,31,12,4,35,8,68,43,2,7,8,68,3,4,45,31,12,4,68,6,49,45,8,68,7,8,54,19,2,68,8,68,21,45,13,68,8,0,12,2,68,14,6,0,63,68,20,25,13,5,44,56,34,7,56,20,7,68,1,56,5,68,16,0,6,35,9,68,12,5,45,13,7,8,10,68,0,12,9,67,68,35,26,24,37,7,8,68,10,36,20,27,3,11,0,68,57,44,26,13,68,58,30,31,68,10,0,7,3,24,46,20,10,68,10,3,39,68,36,45,26,25,1,68,0,68,27,13,4,7,34,14,3,53,26,24,40,7,8,68,10,36,20,27,3,11,0,58,44,26,13,68,29,30,31,68,8,25,17,15,9,68,6,29,54,4,35,68,3,4,45,31,12,4,68,6,49,45,31,12,4,35,8,46,67,13,10,0,58,44,24,37,7,8,68,10,36,20,27,3,11,0,68,57,44,26,13,68,58,30,31,68,54,45,26,27,4,54,67,8,68,9,27,45,8,68,34,30,31,68,46,25,13,68,29,45,31,12,4,35,8,46,67,6,68,12,0,35,54,68,25,49,45,8,68,7,8,54,19,2,68,21,45,13,68,8,0,12,2,68,14,6,0,63]

decrypt :: [Int] -> [Int] -> [Int]
decrypt key msg = zipWith xor msg (cycle key)

score :: [Int] -> Int
score = length . filter (\c -> isAlpha (chr c) || isSpace (chr c) || isPunctuation (chr c) || isDigit (chr c))

solve :: Int
solve = sum bestDecrypted
  where
    keys = [[a, b, c] | a <- [ord 'a'..ord 'z'],
                         b <- [ord 'a'..ord 'z'],
                         c <- [ord 'a'..ord 'z']]
    tryKey k = (score decrypted, decrypted)
      where decrypted = decrypt k cipher
    (_, bestDecrypted) = maximum $ map tryKey keys

main :: IO ()
main = runBench 59 (return solve)
