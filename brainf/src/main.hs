module Main where
import Data.List
import GHC.IO.Handle.Text
import System.IO
import Text.Printf
import Data.Array
import Data.Maybe
import Data.Word
import Data.Char
import System.Environment
import System.Exit

data Operator = Jump Token Int | Operation Token
    deriving (Show, Eq)
data Token =  JumpForwards
            | JumpBackwards
            | DecrementByte 
            | IncrementByte 
            | DecrementPointer 
            | IncrementPointer 
            | Input 
            | Output 
                deriving (Show,Eq)

tokenCharacterTable = [
    (JumpForwards,    '['),
    (JumpBackwards,   ']'),
    (IncrementByte,   '+'),
    (DecrementByte,   '-'),
    (IncrementPointer,'>'),
    (DecrementPointer,'<'),
    (Input,           ','),
    (Output,          '.')]

main = do 
    args <- getArgs
    case args of
        ["help"] -> showHelp
        ["compile",input] -> do 
            file <- readFile input
            output <- return . compile $ file
            flatMapEither (hPutStrLn stderr) (putStrLn) output
        ["compile",inputFile,outputFile] -> do
            input <- readFile inputFile
            output <- return . compile $ input
            flatMapEither (hPutStrLn stderr) (writeFile outputFile) output
        ["interpret","-"] -> getContents >>= execute
        ["interpret",filename] -> readFile filename >>= execute
        _ -> showHelp
        where
            execute c = case (parseJumps . lexer) c of
                Just f  -> run f >> exitSuccess
                Nothing -> putStrLn "parse error" >> exitFailure
            compile :: String -> Either String String
            compile c = case (parseJumps . lexer $ c) of
                Just ops -> Right $ compileToLLVM ops
                Nothing -> Left "failed to parse"

showHelp :: IO ()
showHelp = putStrLn "usage: brainf <filename | '-'>"

flatMapEither :: (a -> c) -> (b -> c) -> Either a b -> c
flatMapEither fLeft _ (Left v) = fLeft v
flatMapEither _ fRight (Right v) = fRight v

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) = case f x of 
    Just v -> v : (filterMap f xs)
    Nothing -> filterMap f xs


toToken :: Char -> Maybe Token
toToken c = find (\(_,x) -> x == c) tokenCharacterTable >>= return . fst

fromToken :: Token -> Char
fromToken t = fromJust (snd <$> (find (\(x,_) -> x == t) tokenCharacterTable))

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) = case toToken c of
    Just t  -> t : lexer cs
    Nothing -> lexer cs

parseJumps :: [Token] -> Maybe [Operator]
parseJumps ts = parseJumpsHelper [] (zip [0..] ts) >>= return . fst 

parseJumpsHelper :: [Int] -> [(Int,Token)] -> Maybe ([Operator],[Int])
-- forwards jump found
parseJumpsHelper forwardJumpStack ((i,JumpForwards):ts) = do
    (output,backwardsJumpStack) <- parseJumpsHelper (i:forwardJumpStack) ts
    (matchingJump,remainingStack) <- uncons backwardsJumpStack
    return ((Jump JumpForwards (matchingJump+1):output),remainingStack)
-- backwards jump found
parseJumpsHelper forwardJumpStack ((i,JumpBackwards):ts) = do
    (matchingJump,remainingStack) <- uncons forwardJumpStack
    (parsedEnd,backwardsJumpStack) <- parseJumpsHelper remainingStack ts
    -- +1 so we go past the [ and dont get stuck
    return ((Jump JumpBackwards (matchingJump+1)) : parsedEnd,i:backwardsJumpStack)
-- any other token found
parseJumpsHelper forwardJumpStack ((i,t):ts) = 
    (\(o,s) -> ((Operation t):o,s)) <$> parseJumpsHelper forwardJumpStack ts
-- base case
parseJumpsHelper _ [] = Just ([],[])

run :: [Operator] -> IO ()
run ts = evalLoop ts (listArray (0,255) ([0..255] >> [0])) 0 0 

evalLoop :: [Operator] -> Array Int Word8 -> Int -> Int -> IO ()
evalLoop ts mem ip dp
    | (length) ts > ip = case ts !! ip of
        Operation IncrementByte    -> evalLoop ts (mem // [(dp,(mem ! dp) + 1)]) (ip+1) dp
        Operation DecrementByte    -> evalLoop ts (mem // [(dp,(mem ! dp) - 1)]) (ip+1) dp
        Operation IncrementPointer -> evalLoop ts mem (ip+1) (dp+1)
        Operation DecrementPointer -> evalLoop ts mem (ip+1) (dp-1)
        Operation Input            -> inputByte >>= \b -> evalLoop ts (mem // [(dp,b)]) (ip+1) dp
        Operation Output           -> outputByte (mem ! dp) >> evalLoop ts mem (ip+1) dp
        Jump JumpForwards location  -> if (mem ! dp) == 0
            then evalLoop ts mem location dp
            else evalLoop ts mem (ip+1) dp
        Jump JumpBackwards location -> if (mem ! dp) /= 0
            then evalLoop ts mem location dp
            else evalLoop ts mem (ip+1) dp
    | otherwise = return ()

outputByte :: Word8 -> IO ()
outputByte = putChar . chr . fromIntegral

inputByte :: IO Word8
inputByte = getChar >>= return . fromIntegral . ord

testData = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

compileToLLVM :: [Operator] -> String
compileToLLVM ops = (join '\n') . flatten $ [[""],functions,[""],globals,[""],generateMain ops]

join :: Char -> [String] -> String
join seperator [] = []
join seperator (x:[]) = (x ++ [seperator])
join seperator (x:xs) = (x ++ [seperator]) ++ join seperator xs

globals = ["; globals"]

functions = ["; libc declarations","declare i32 @putchar(i32) nounwind"]

generateMain :: [Operator] -> [String]
generateMain ops = 
    ["define i32 @main() {"] ++ 
    [indent "; ====== initial setup ======"] ++
    (indent <$> initialMainSetup) ++ 
    [indent "; ====== main program ======"] ++
    (indent <$> (enumerate ops >>= generateLLVMForOp)) ++ 
    [indent "; ====== terminate program ======"] ++
    (indent <$> mainTermination) ++
    ["}"]

indent :: String -> String
indent = ('\t' :)

mainTermination = ["ret i32 0"]

initialMainSetup =
    ["; main mem",
     "%memory = alloca [1024 x i8]",
     "call void @llvm.memset.p0.i64(ptr %memory, i8 0, i64 1024, i1 false)",
     "; data pointer",
     "%dp = alloca ptr",
     "store ptr %memory, ptr %dp"                                       ]

generateJumpLabel :: Int -> [String]
generateJumpLabel i = 
    ["br label %instruction_" ++ show i] ++
    ["instruction_" ++ show i ++ ":"]

enumerate = zip [0..]

generateLLVMForOp :: (Int,Operator) -> [String]
generateLLVMForOp (i,op) = getLLVMforOp (i,op)

getLLVMforOp :: (Int,Operator) -> [String]
getLLVMforOp (i,Operation IncrementByte) = 
    [ printf "; increment value",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%byte.%d = load i8, ptr %%ptr.%d" i i,
      printf "%%byte.new.%d = add i8 1, %%byte.%d" i i,
      printf "store i8 %%byte.new.%d, ptr %%ptr.%d" i i    ]
getLLVMforOp (i,Operation DecrementByte) = 
    [ printf "; decrement value",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%byte.%d = load i8, ptr %%ptr.%d" i i,
      printf "%%byte.new.%d = sub i8 %%byte.%d, 1" i i,
      printf "store i8 %%byte.new.%d, ptr %%ptr.%d" i i    ]
getLLVMforOp (i,Operation IncrementPointer) = 
    [ printf "; shift data pointer right",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%dp.new.%d = getelementptr [2 x i8], ptr %%ptr.%d, i64 0, i64 1" i i,
      printf "store ptr %%dp.new.%d, ptr %%dp" i                        ]
getLLVMforOp (i,Operation DecrementPointer) = 
    [ printf "; shift data pointer left",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%dp.new.%d = getelementptr [2 x i8], ptr %%ptr.%d, i64 0, i64 -1" i i,
      printf "store ptr %%dp.new.%d, ptr %%dp" i                        ]
getLLVMforOp (i,Operation Output) = 
    [ printf "; print byte",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%byte.%d = load i8, ptr %%ptr.%d" i i,
      printf "%%int.%d = sext i8 %%byte.%d to i32" i i,
      printf "call i32 @putchar(i32 %%int.%d)" i ]
getLLVMforOp (i,Jump JumpForwards pos) = 
    [ printf "; forwards jump",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%byte.%d = load i8, ptr %%ptr.%d" i i,
      printf "%%condition.%d = icmp eq i8 %%byte.%d, 0" i i,
      printf "br i1 %%condition.%d, label %%instruction_%d, label %%instruction_%d" i pos (i+1),
      printf "instruction_%d:" (i+1)                                                            ]
getLLVMforOp (i,Jump JumpBackwards pos) = 
    [ printf "; backwards jump",
      printf "%%ptr.%d = load ptr, ptr %%dp" i,
      printf "%%byte.%d = load i8, ptr %%ptr.%d" i i,
      printf "%%condition.%d = icmp ne i8 %%byte.%d, 0" i i,
      printf "br i1 %%condition.%d, label %%instruction_%d, label %%instruction_%d" i pos (i+1),
      printf "instruction_%d:" (i+1)                                                            ]
getLLVMforOp _ = []

flatten :: [[a]] -> [a]
flatten = (>>= id)

-- >>> flatten [[1,2],[3,4]]
-- [1,2,3,4]

-- >>> run . parseDepth . lexer $ ['+' | _ <- [1..97]] ++ "." ++  ">" ++ ['+' | _ <- [1..10]] ++ "."

-- >>> putStrLn "epic"
--
-- >>> (parseDepth . lexer $ "[[[]]][]")
-- Just [Jump JumpForwards 6,Jump JumpForwards 5,Jump JumpForwards 4,Jump JumpBackwards 3,Jump JumpBackwards 2,Jump JumpBackwards 1,Jump JumpForwards 8,Jump JumpBackwards 7]

-- >>> parseDepth . lexer $ testData
-- [Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Nested JumpForwards 0,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Nested JumpForwards 1,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementByte,Nested JumpBackwards 1,Flat IncrementPointer,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementPointer,Flat DecrementByte,Flat IncrementPointer,Flat IncrementPointer,Flat IncrementByte,Nested JumpForwards 1,Flat DecrementPointer,Nested JumpBackwards 1,Flat DecrementPointer,Flat DecrementByte,Nested JumpBackwards 0,Flat IncrementPointer,Flat IncrementPointer,Flat Output,Flat IncrementPointer,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat IncrementPointer,Flat IncrementPointer,Flat Output,Flat DecrementPointer,Flat DecrementByte,Flat Output,Flat DecrementPointer,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat IncrementPointer,Flat IncrementPointer,Flat IncrementByte,Flat Output,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat Output]
