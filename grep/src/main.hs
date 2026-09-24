module Main where

import qualified System.Environment as SysEnv
import System.IO

main = do
    args <- SysEnv.getArgs
    case args of
        (arg1:arg2:_) -> do
            lines <- readFileLines arg2
            sequence_ $ map putStrLn $ map formatResult (filter (\(_,l) -> doesLineContain arg1 l) (enumerate lines))
        _ -> hPutStr stderr "Bad arguments\nUsage: grep [pattern] [file]\n"

formatResult (i,x) = "Line " ++ show (i+1) ++ ": " ++ x

doesLineContain [] [] = True
doesLineContain [] x = True
doesLineContain substr [] = False
doesLineContain substr (c:cs) =
    if c == (first substr) then doesLineContain (tail substr) cs
    else doesLineContain substr cs

enumerate :: [a] -> [(Int,a)]
enumerate l = myZip [0..] l

myZip :: [a] -> [b] -> [(a,b)]
myZip la [] = []
myZip [] lb = []
myZip (a:la) (b:lb) =  [(a,b)] ++ myZip la lb

first (x:xs) = x

readFileLines :: FilePath -> IO [String]
readFileLines path = do
    lines <- readFile path
    return (splitByNewline lines)

splitByNewline :: String -> [String]
splitByNewline ('\n':cs) = [[]] ++ (splitByNewline cs)
splitByNewline (c:cs) = addToFront c (splitByNewline cs)
splitByNewline c = [c]

addToFront :: Char -> ([String] -> [String])
addToFront c = \strings -> [[c] ++ (head strings)] ++ (tail strings);

takeAllButLast :: [n] -> [n]
takeAllButLast [last] = []
takeAllButLast (item:others) = [item] ++ (takeAllButLast others)
