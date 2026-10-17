module Main where
import System.Environment as SysEnv
import Text.Read hiding (lex)
import Prelude hiding (lex,takeWhile)
import System.Exit
import Data.Char

data Token = Literal Double | Operator Operator deriving (Show,Eq)
data Operator = Add | Sub | Div | Mult | LeftBracket | RightBracket deriving (Show,Eq)

main = do
    args <- SysEnv.getArgs
    case args of
        [arg1] -> case calculate arg1 of
            Right result -> print result >> exitSuccess
            Left e -> putStrLn e >> exitFailure
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
-- deal with minus sign association with literals
lex (b:c:leftover)
    | isNumber c && b == '-' = case (reads (b:c:leftover) :: [(Double,String)]) of
        [(x,unparsed)] -> fmap (\ts -> Literal x : ts) $ lex unparsed
        _ -> Left "error parsing double"
-- parse single char operators
lex (c:cs)
    | '0' <= c && c <= '9' = case (reads (c:cs) :: [(Double,String)]) of
        -- add a space so that any minus sign directly following a literal is treated as an operator
        [(x,'-':unparsed)] -> fmap (\ts -> Literal x : ts) $ lex $ '-':' ':unparsed
        [(x,unparsed)] -> fmap (\ts -> Literal x : ts) $ lex unparsed
        _ -> Left "error parsing double"
-- parse brackets
lex ('(':cs) = fmap (\xs -> (Operator LeftBracket):xs) $ lex cs
lex (')':cs) = fmap (\xs -> (Operator RightBracket):xs) $ lex cs
-- parse an operator
lex (c:cs) = case parseOperator c of
    Right op -> fmap (\x -> (Operator op) : x) $ lex cs
    Left e -> Left e

-- >>> getRight . lex $ "10*(-4)"
-- [Literal 10.0,Operator LeftBracket,Literal (-4.0),Operator RightBracket]

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

instance Ord Token where
    Literal a >= Literal b = a >= b
    Operator a >= Operator b = a >= b
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
shuntHelper stack [] output = Right $ output ++ stack
shuntHelper stack ((Operator LeftBracket):ts) output = shuntHelper ((Operator LeftBracket):stack) ts output
shuntHelper stack ((Operator RightBracket):ts) output = result where
    newStack = case splitOnce (Operator LeftBracket) stack of
        Nothing -> Left "missing opening bracket"
        Just newStack -> Right newStack
    result = case newStack of
        Left e -> Left e
        Right (l,r) -> shuntHelper r ts (pushStack l output)
shuntHelper stack ((Operator op):ts) output = result where
    (opsToPush,newStack) = takeWhile (>= Operator op) stack
    newOutput = pushStack opsToPush output
    result = shuntHelper ((Operator op):newStack) ts newOutput
shuntHelper stack ((Literal value):ts) output = shuntHelper (stack) ts $ output ++ [Literal value]

-- >>> splitOnce (Operator LeftBracket) [Literal 1, Operator LeftBracket, Literal 2]
-- Just ([Literal 1.0],[Literal 2.0])

-- >>> shunt $ getRight $ lex "1 + 1 * 2"
-- Right [Literal 1.0,Literal 1.0,Literal 2.0,Operator Mult,Operator Add]

-- >>> shunt $ getRight $ lex "1+(-5)+1"
-- Right [Literal 1.0,Literal 5.0,Operator Sub,Operator Add,Literal 1.0,Operator Add]

-- >>> shunt $ getRight $ lex "(1 +2) * 3"
-- Right [Literal 1.0,Literal 2.0,Operator Add,Literal 3.0,Operator Mult,Operator LeftBracket]

-- >>>shunt [Literal 1.0, Operator Add]

-- >>> takeWhile (>= Operator Mult) [Operator Add]
-- ([],[Operator Add])

-- >>> pushStack [] [Literal 1]
-- [Literal 1.0]

pushStack :: [a] -> [a] -> [a]
pushStack (x:xs) stack = pushStack xs (stack ++ [x])
pushStack [] stack = stack

maybeHead :: [a] -> Maybe a
maybeHead (head:tail) = Just head
maybeHead [] = Nothing

takeWhile :: Ord a => (a -> Bool) -> [a] -> ([a],[a])
takeWhile p [] = ([],[])
takeWhile p (x:xs)
    | p x       = (\(l,r) -> (x:l,r)) $ takeWhile p xs
    | otherwise = ([],x:xs)

-- >>> takeWhile (<= 1) [1,2]
-- ([1],[2])

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
-- sub with one operand is making a number negative
applyOperator Sub [first] = Right [0 - first]
-- implicit 0 or 1 for operators without both operands
applyOperator Add [first] = Right $ first:[]
applyOperator operator [first] = Right $ ((getOperatorFunc operator) first 1):[]
-- propper applicatino with 2 opperands
applyOperator operator (first:second:others) = Right $ ((getOperatorFunc operator) second first):others
applyOperator operator _ = Left "stack empty when applying operator"

-- >>> eval $ getRight . shunt . getRight $ lex "1 - 2"
-- Right (-1.0)

-- >>> shunt . getRight $ lex "1 - 2"
-- Right [Literal 1.0,Literal 2.0,Operator Sub]

-- >>> getRight . shunt . getRight $ lex "5*(-3)"
-- [Literal 5.0,Literal 3.0,Operator Sub,Operator Mult]

-- >>> (-) 2 1
-- 1
