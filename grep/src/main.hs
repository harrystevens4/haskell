module Main where

import qualified System.Environment as SysEnv

main = do
    args <- SysEnv.getArgs
    let (arg1:arg2:_) = args
    lines <- readFileLines arg2
    print (filter (\(_,l) -> doesLineContain arg1 l) (enumerate lines))

doesLineContain [] [] = True
doesLineContain [] x = True
doesLineContain substr [] = False
doesLineContain substr (c:cs) =
    if c == (first substr) then doesLineContain (tail substr) cs
    else doesLineContain substr cs

enumerate :: [a] -> [(Int,a)]
enumerate [] = []
enumerate [x] = [(0,x)]
enumerate (x:xs) = [(0,x)] ++ map (\(n,x) -> (n+1,x)) (enumerate xs)

first (x:xs) = x

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

--last [x] = x
--last (x:xs) = last xs

takeAllButLast :: [n] -> [n]
takeAllButLast [last] = []
takeAllButLast (item:others) = [item] ++ (takeAllButLast others)
