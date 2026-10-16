module Main where
import System.Environment as SysEnv
import Text.Read hiding (lex)
import Prelude hiding (lex,takeWhile)

data Token = Literal Double | Operator Operator deriving (Show,Eq,Ord)
data Operator = Add | Sub | Div | Mult | LeftBracket | RightBracket deriving (Show,Eq)

main = do
    args <- SysEnv.getArgs
    case args of
        [arg1] -> case calculate arg1 of
            Right result -> print result
            Left e -> putStrLn e
        _ -> putStrLn "Usage: csfc \"expression\""

calculate :: String -> Either String Double
calculate expr = case lex expr of
    Left e -> Left ("parse error: " ++ e)
    Right tokens -> case shunt tokens of
        Left e -> Left ("shunt error: " ++ e)
        Right tokens -> case eval tokens of
            Left e -> Left ("eval error: " ++ e)
            Right result -> Right (result)

lex :: String -> Either String [Token]
lex [] = Right []
-- ignore whitespace
lex (' ':cs) = lex cs
-- parse single char operators
lex (c:cs)
    | '0' <= c && c <= '9' = case (reads (c:cs) :: [(Double,String)]) of
        [(x,unparsed)] -> fmap (\ts -> Literal x : ts) $ lex unparsed
        _ -> Left "error parsing double"
-- parse brackets
lex ('(':cs) = fmap (\xs -> (Operator LeftBracket):xs) $ lex cs
lex (')':cs) = fmap (\xs -> (Operator RightBracket):xs) $ lex cs
-- parse an operator
lex (c:cs) = case parseOperator c of
    Right op -> fmap (\x -> (Operator op) : x) $ lex cs
    Left e -> Left e

rightOr :: b -> Maybe a -> Either b a
rightOr b (Just a) = Right a
rightOr b Nothing = Left b

-- >>> lex "1 2 +"
-- Right [Literal 1.0,Literal 2.0,Operator Add]

-- >>> (reads "1.1") :: [(Double,String)]
-- [(1.1,"")]

parseOperator :: Char -> Either String Operator
parseOperator '+' = Right Add
parseOperator '-' = Right Sub
parseOperator '/' = Right Div
parseOperator '*' = Right Mult
parseOperator unparsable = Left $ "could not parse operator from \"" ++ [unparsable] ++ "\""

instance Ord Operator where
    Add >= Add = True
    Add >= Sub = True
    Sub >= Add = True
    Sub >= Sub = True
    Div >= _   = True
    Mult >= _  = True
    _ >= _ = False

shunt :: [Token] -> Either String [Token]
shunt ts = shuntHelper [] ts []

--              stack      input      output
shuntHelper :: [Token] -> [Token] -> [Token] -> Either String [Token]
shuntHelper stack ((Operator LeftBracket):ts) output = shuntHelper ((Operator LeftBracket):stack) ts output
shuntHelper stack ((Operator RightBracket):ts) output = result where
    newStack = case splitOnce (Operator RightBracket) stack of
        Nothing -> Left "missing closing bracket"
        Just newStack -> Right newStack
    result = case newStack of
        Left e -> Left e
        Right (l,r) -> shuntHelper ((Operator LeftBracket):r) ts (pushStack l output)
shuntHelper stack ((Operator op):ts) output = result where
    (opsToPush,newStack) = takeWhile (>= Operator op) stack
    output = pushStack opsToPush output
    result = shuntHelper ((Operator op):newStack) ts output
shuntHelper stack ((Literal value):ts) output = shuntHelper ((Literal value):stack) ts output
shuntHelper stack [] output = Right $ output ++ stack
--
-- >>> shunt $ getRight $ lex "1 + 2"
-- Right [Literal 1.0]

pushStack :: [a] -> [a] -> [a]
pushStack (x:xs) stack = pushStack xs (x:stack)
pushStack [] stack = stack

maybeHead :: [a] -> Maybe a
maybeHead (head:tail) = Just head
maybeHead [] = Nothing

takeWhile :: Ord a => (a -> Bool) -> [a] -> ([a],[a])
takeWhile p [] = ([],[])
takeWhile p (x:xs)
    | p x       = (\(l,r) -> (x:l,r)) $ takeWhile p xs
    | otherwise = ([],x:xs)

-- >>> takeWhile (<= 5) [1..10]
-- ([1,2,3,4,5],[6,7,8,9,10])

splitOnce :: Eq a => a -> [a] -> Maybe ([a],[a])
splitOnce deliminator (top:base)
    | top == deliminator = Just ([],base)
    | otherwise          = fmap (\(l,r) -> (top:l,r)) $ splitOnce deliminator base
splitOnce deliminator [] = Nothing

-- >>> splitOnce '=' "this=that"
-- Just ("this","that")

eval :: [Token] -> Either String Double
eval ts = evalHelper [] ts

-- >>> eval $ getRight $ lex "1 2 + 3 4 + *"
-- Right 21.0

getRight :: Either a b -> b
getRight (Right b) = b

evalHelper :: [Double] -> [Token] -> Either String Double
-- base cases
evalHelper [result] [] = Right result
evalHelper stack    [] = Left "result of calculation did not yield 1 item on the stack"
-- stack evaluating
evalHelper stack ((Literal x):ts) = evalHelper (x:stack) ts
evalHelper stack ((Operator op):ts) = case applyOperator op stack of
    Left e -> Left e
    Right newStack -> evalHelper newStack ts

getOperatorFunc :: Operator -> Double -> Double -> Double
getOperatorFunc Add = (+)
getOperatorFunc Sub = (-)
getOperatorFunc Mult = (*)
getOperatorFunc Div = (/)

applyOperator :: Operator -> [Double] -> Either String [Double]
applyOperator operator (first:second:others) = Right $ ((getOperatorFunc operator) first second):others
applyOperator operator _ = Left "stack empty when applying operator"
