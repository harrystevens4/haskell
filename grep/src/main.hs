module Main where

import qualified System.Environment as SysEnv

main = do
    args <- SysEnv.getArgs
    fileContents <- readFile (head args)
    print (splitByNewline fileContents)

readFileLines path = do
    readFile path

splitByNewline :: String -> [String]
splitByNewline ('\n':cs) = (splitByNewline cs) ++ [[]]
splitByNewline (c:cs) = addToHead c (splitByNewline cs)
splitByNewline c = [c]

addToHead :: Char -> ([String] -> [String])
addToHead c = \strings -> (takeAllButLast strings) ++ [(last strings) ++ [c]]

takeAllButLast :: [n] -> [n]
takeAllButLast [last] = []
takeAllButLast (item:others) = [item] ++ (takeAllButLast others)
