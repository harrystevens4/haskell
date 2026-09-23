module Main where

import qualified System.Environment as SysEnv

main = do
    args <- SysEnv.getArgs
    lines <- readFileLines (head args)
    print lines

readFileLines :: FilePath -> IO [String]
readFileLines path = do
    lines <- readFile path
    return (splitByNewline lines)

splitByNewline :: String -> [String]
splitByNewline ('\n':cs) = [[]] ++ (splitByNewline cs)
splitByNewline (c:cs) = addToFront c (splitByNewline cs)
splitByNewline c = [c]

--addToHead :: Char -> ([String] -> [String])
--addToHead c = \strings -> (takeAllButLast strings) ++ [[c] ++(last strings)]

addToFront :: Char -> ([String] -> [String])
addToFront c = \strings -> [[c] ++ (head strings)] ++ (tail strings);

takeAllButLast :: [n] -> [n]
takeAllButLast [last] = []
takeAllButLast (item:others) = [item] ++ (takeAllButLast others)
