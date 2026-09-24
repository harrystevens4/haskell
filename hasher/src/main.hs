module Main where

import Data.Char

main :: IO ()
main = do
    input <- getContents
    putStrLn . intToHex . hash $ input

hash :: String -> Int
hash [] = 0x1f2e3d4c
hash [c] = ((ord c * 0x67676767) `div` 13) - 589723462
hash (c:cs) = hash [c] + hash cs

intToHex :: Int -> String
intToHex x | x < 16 = [getNth x (['0'..'9'] ++ ['a'..'f'])]
intToHex x = intToHex (x `div` 16) ++ intToHex (x `mod` 16)

getNth :: Int -> [a] -> a
getNth 0 (a:as) = a
getNth n (a:as) = getNth (n-1) as

