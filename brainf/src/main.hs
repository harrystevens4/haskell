module Main where
import Data.List
import Data.Array
import Data.Maybe
import Data.Word
import Data.Char

data Operator = Nested Token Int | Flat Token 
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

main = getContents >>= pure . parseDepth . lexer >>= run

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

parseDepth :: [Token] -> [Operator]
parseDepth = parseDepthHelper 0

parseDepthHelper :: Int -> [Token] -> [Operator]
parseDepthHelper depth (JumpForwards:ts) = 
    (Nested JumpForwards depth) : parseDepthHelper (depth+1) ts
parseDepthHelper depth (JumpBackwards:ts) = 
    (Nested JumpBackwards (depth-1)) : parseDepthHelper (depth-1) ts
parseDepthHelper depth (t:ts) = Flat t : parseDepthHelper depth ts
parseDepthHelper depth [] = []

run :: [Operator] -> IO ()
run ts = evalLoop ts (listArray (0,255) ([0..255] >> [0])) 0 0 

evalLoop :: [Operator] -> Array Int Word8 -> Int -> Int -> IO ()
evalLoop ts mem ip dp
    | (length) ts > ip = case ts !! ip of
        Flat IncrementByte    -> evalLoop ts (mem // [(dp,(mem ! dp) + 1)]) (ip+1) dp
        Flat DecrementByte    -> evalLoop ts (mem // [(dp,(mem ! dp) - 1)]) (ip+1) dp
        Flat IncrementPointer -> evalLoop ts mem (ip+1) (dp+1)
        Flat DecrementPointer -> evalLoop ts mem (ip+1) (dp-1)
        Flat Input            -> inputByte >>= \b -> evalLoop ts (mem // [(dp,b)]) (ip+1) dp
        Flat Output           -> outputByte (mem ! dp) >> evalLoop ts mem (ip+1) dp
        Nested JumpForwards depth  -> if (mem ! dp) == 0
            then evalLoop ts mem (matchingJumpBack+1) dp
            else evalLoop ts mem (ip+1) dp
                where
                    matchingJumpBack = fromJust $ elemIndex (Nested JumpBackwards depth) endList
                    endList = drop ip ts
        Nested JumpBackwards depth -> if (mem ! dp) /= 0
            then evalLoop ts mem (matchingJumpForward+1) dp
            else evalLoop ts mem (ip+1) dp
                where
                    matchingJumpForward = last $ elemIndices (Nested JumpForwards depth) (startList)
                    startList = take ip ts
    | otherwise = return ()

outputByte :: Word8 -> IO ()
outputByte = putChar . chr . fromIntegral

inputByte :: IO Word8
inputByte = getChar >>= return . fromIntegral . ord

testData = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

-- >>> run . parseDepth . lexer $ ['+' | _ <- [1..97]] ++ "." ++  ">" ++ ['+' | _ <- [1..10]] ++ "."

-- >>> putStrLn "epic"
--
-- >>> last $ elemIndices (Nested JumpForwards 0) (take 14 (parseDepth . lexer $ "[]>[]++++++++++[.]"))
-- 12

-- >>> parseDepth . lexer $ testData
-- [Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Nested JumpForwards 0,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Nested JumpForwards 1,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementByte,Nested JumpBackwards 1,Flat IncrementPointer,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementPointer,Flat DecrementByte,Flat IncrementPointer,Flat IncrementPointer,Flat IncrementByte,Nested JumpForwards 1,Flat DecrementPointer,Nested JumpBackwards 1,Flat DecrementPointer,Flat DecrementByte,Nested JumpBackwards 0,Flat IncrementPointer,Flat IncrementPointer,Flat Output,Flat IncrementPointer,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat IncrementPointer,Flat IncrementPointer,Flat Output,Flat DecrementPointer,Flat DecrementByte,Flat Output,Flat DecrementPointer,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat IncrementPointer,Flat IncrementPointer,Flat IncrementByte,Flat Output,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat Output]
