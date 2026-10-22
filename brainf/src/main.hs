module Main where
import Data.List
import Data.Array
import Data.Maybe
import Data.Word
import Data.Char

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
    file <- getContents
    case parseDepth . lexer $ file of
        (Just result) -> run result
        Nothing -> putStrLn "failed to parse"

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

parseDepth :: [Token] -> Maybe [Operator]
parseDepth ts = parseDepthHelper [] (zip [0..] ts) >>= return . fst 

parseDepthHelper :: [Int] -> [(Int,Token)] -> Maybe ([Operator],[Int])
-- forwards jump found
parseDepthHelper forwardJumpStack ((i,JumpForwards):ts) = do
    (output,backwardsJumpStack) <- parseDepthHelper (i:forwardJumpStack) ts
    (matchingJump,remainingStack) <- uncons backwardsJumpStack
    return ((Jump JumpForwards (matchingJump+1):output),remainingStack)
-- backwards jump found
parseDepthHelper forwardJumpStack ((i,JumpBackwards):ts) = do
    (matchingJump,remainingStack) <- uncons forwardJumpStack
    (parsedEnd,backwardsJumpStack) <- parseDepthHelper remainingStack ts
    -- +1 so we go past the [ and dont get stuck
    return ((Jump JumpBackwards (matchingJump+1)) : parsedEnd,i:backwardsJumpStack)
-- any other token found
parseDepthHelper forwardJumpStack ((i,t):ts) = 
    (\(o,s) -> ((Operation t):o,s)) <$> parseDepthHelper forwardJumpStack ts

-- base case
parseDepthHelper _ [] = Just ([],[])

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

-- >>> run . parseDepth . lexer $ ['+' | _ <- [1..97]] ++ "." ++  ">" ++ ['+' | _ <- [1..10]] ++ "."

-- >>> putStrLn "epic"
--
-- >>> (parseDepth . lexer $ "[[[]]][]")
-- Just [Jump JumpForwards 6,Jump JumpForwards 5,Jump JumpForwards 4,Jump JumpBackwards 3,Jump JumpBackwards 2,Jump JumpBackwards 1,Jump JumpForwards 8,Jump JumpBackwards 7]

-- >>> parseDepth . lexer $ testData
-- [Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Nested JumpForwards 0,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Nested JumpForwards 1,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementPointer,Flat DecrementByte,Nested JumpBackwards 1,Flat IncrementPointer,Flat IncrementByte,Flat IncrementPointer,Flat IncrementByte,Flat IncrementPointer,Flat DecrementByte,Flat IncrementPointer,Flat IncrementPointer,Flat IncrementByte,Nested JumpForwards 1,Flat DecrementPointer,Nested JumpBackwards 1,Flat DecrementPointer,Flat DecrementByte,Nested JumpBackwards 0,Flat IncrementPointer,Flat IncrementPointer,Flat Output,Flat IncrementPointer,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat IncrementPointer,Flat IncrementPointer,Flat Output,Flat DecrementPointer,Flat DecrementByte,Flat Output,Flat DecrementPointer,Flat Output,Flat IncrementByte,Flat IncrementByte,Flat IncrementByte,Flat Output,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat DecrementByte,Flat Output,Flat IncrementPointer,Flat IncrementPointer,Flat IncrementByte,Flat Output,Flat IncrementPointer,Flat IncrementByte,Flat IncrementByte,Flat Output]
