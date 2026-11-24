{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Char
import System.Environment as SysEnv
import System.Exit
import System.IO
import Text.Read hiding (lex)
import Prelude hiding (lex, takeWhile)

data Token = Literal Double | Operator Operator deriving (Show, Eq)

data Operator
    = Add
    | Sub
    | Div
    | Mult
    | Pow
    | Fact
    | Sin
    | Cos
    | Tan
    | LeftBracket
    | RightBracket
    deriving (Show, Eq)

newtype Parser a = Parser (String -> Either String (a, String))

infixl 9 <||>
(<||>) :: Parser a -> Parser a -> Parser a
Parser f <||> Parser g = Parser $ \x -> f x `eitherOr` g x

eitherOr :: Either e a -> Either e a -> Either e a
eitherOr (Right a) (_) = Right a
eitherOr (Left e) (b) = b

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser g) =
        Parser
            ( \x -> case g x of
                Left e -> Left e
                Right x -> Right $ (mapFst f x)
            )

instance Applicative Parser where
    pure a = Parser (const $ Right (a, ""))
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser f <*> Parser g =
        Parser
            ( \x -> case f x of
                Right (f', _) -> fmap (mapFst f') (g x)
                Left e -> Left e
            )

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser f >>= g = Parser $ \x -> case (f x) of
        Left e -> Left e
        Right (r, s) -> parseInput x (g r)

parseInput :: String -> Parser a -> Either String (a, String)
parseInput s (Parser f) = f s

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

unaryOps =
    [ Fact
    , Sin
    , Cos
    , Tan
    ]

postfixOps =
    [ Fact
    ]

prefixOps =
    [ Sin
    , Cos
    , Tan
    ]

-- binaryOps :: (Operator, Operator, Operator, Operator, Operator)
binaryOps =
    [ Add
    , Sub
    , Div
    , Mult
    , Pow
    ]

main = do
    args <- SysEnv.getArgs
    case args of
        [arg1] -> calculateThenPrint arg1
        [] -> getContents >>= calculateThenPrint
        _ -> putStrLn "Usage: csfc \"expression\""

-- evaluate the expression then print the result or error
calculateThenPrint :: String -> IO ()
calculateThenPrint expr = case calculate expr of
    Right result -> print result >> exitSuccess
    Left e -> hPutStrLn stderr e >> exitFailure

calculate :: String -> Either String Double
-- such sexy syntax
calculate expr = lex expr >>= shunt >>= eval

parseConstant :: Parser Token
parseConstant = fmap Literal $ Parser $ \case
    ('p' : 'i' : cs) -> Right (pi, cs)
    cs -> Left $ "unknown constant " ++ show cs

parseFunction :: Parser Token
parseFunction = fmap Operator $ Parser $ \case
    ('s' : 'i' : 'n' : cs) -> Right (Sin, cs)
    ('c' : 'o' : 's' : cs) -> Right (Cos, cs)
    ('t' : 'a' : 'n' : cs) -> Right (Tan, cs)
    cs -> Left $ "unknown function " ++ show cs

parseOperator :: Parser Token
parseOperator = fmap Operator <$> Parser $ \case
    ('(' : cs) -> Right (LeftBracket, cs)
    (')' : cs) -> Right (RightBracket, cs)
    ('+' : cs) -> Right (Add, cs)
    ('-' : cs) -> Right (Sub, cs)
    ('/' : cs) -> Right (Div, cs)
    ('*' : cs) -> Right (Mult, cs)
    ('^' : cs) -> Right (Pow, cs)
    ('!' : cs) -> Right (Fact, cs)
    (c : _) -> Left $ "single character " ++ show c ++ " did not match known operator"

parseLiteral :: Parser Token
parseLiteral = fmap Literal $ Parser $ \x -> case reads x of
    (x : _) -> Right x
    [] -> Left $ "unknown literal " ++ show x

-- im up in the club parsing it and by it i mean my lexemes
lex :: String -> Either String [Token]
lex [] = Right []
-- ignore whitespace
lex (' ' : cs) = lex cs
lex ('\n' : cs) = lex cs
lex ('\r' : cs) = lex cs
lex ('\t' : cs) = lex cs
-- lex cs = fmap (\(x, xs) -> fmap (Operator x :) (lex xs)) $ parseInput cs parseSingleCharOperator
lex cs = do
    (op, remaining) <-
        parseInput
            cs
            ( parseOperator
                <||> parseFunction
                <||> parseConstant
                <||> parseLiteral
            )
    ops <- lex remaining
    pure (op : ops)

-- >>> lex "(1+3) * (4/2)" >>= shunt
-- Left "missing opening bracket"

-- >>> parseInput "(1+2+3)" (parseOperator <||> parseConstant)
-- Right (Operator LeftBracket,"1+2+3)")

-- -- deal with minus sign association with literals
-- lex (b : c : leftover)
--     | isNumber c && b == '-' = case (reads (b : c : leftover) :: [(Double, String)]) of
--         [(x, unparsed)] -> fmap (\ts -> Literal x : ts) $ lex unparsed
--         _ -> Left "error parsing double"
-- -- parse single char operators
-- lex (c : cs)
--     | isNumber c = case (reads (c : cs) :: [(Double, String)]) of
--         -- add a space so that any minus sign directly following a literal is treated as an operator
--         [(x, '-' : unparsed)] -> (\ts -> Literal x : ts) <$> (lex $ '-' : ' ' : unparsed)
--         [(x, unparsed)] -> (\ts -> Literal x : ts) <$> (lex unparsed)
--         _ -> Left "error parsing double"
-- -- parse brackets
-- lex ('(' : cs) = (\xs -> (Operator LeftBracket) : xs) <$> (lex cs)
-- lex (')' : cs) = (\xs -> (Operator RightBracket) : xs) <$> (lex cs)
-- -- parse an operator
-- lex (c : cs) = parseOperator c >>= \op -> (\x -> (Operator op) : x) `fmap` (lex cs)

-- >>> getRight . lex $ "(1-4)"
-- [Operator LeftBracket,Literal 1.0,Operator Sub,Literal 4.0,Operator RightBracket]

-- cant find in standard library?
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e (Just x) = Right x
maybeToEither e (Nothing) = Left e

-- >>> lex "1 2 +"
-- Right [Literal 1.0,Literal 2.0,Operator Add]

-- >>> (reads "1.1") :: [(Double,String)]
-- [(1.1,"")]

-- higher binding power is higher order of operations precidence
operatorBindingPower :: Operator -> Int
operatorBindingPower Add = 1
operatorBindingPower Sub = 1
operatorBindingPower Mult = 2
operatorBindingPower Div = 2
operatorBindingPower Pow = 3
operatorBindingPower Fact = 3
operatorBindingPower _ = 4

-- compare by binding power
instance Ord Token where
    Literal a >= Literal b = a >= b
    Operator a >= Operator b = a >= b
    Literal a <= Literal b = a <= b
    Operator a <= Operator b = a <= b

instance Ord Operator where
    x >= y = operatorBindingPower x >= operatorBindingPower y
    x <= y = operatorBindingPower x <= operatorBindingPower y

shunt :: [Token] -> Either String [Token]
-- start with an empty stack and output
shunt ts = shuntHelper [] ts []

-- shunting yard algorithm to convert infix into postfix
--              stack      input      output
shuntHelper :: [Token] -> [Token] -> [Token] -> Either String [Token]
-- pop stack onto output once we run out of input tokens
shuntHelper stack [] output = Right $ output ++ stack
shuntHelper stack (Operator op : ts) output
    | op `elem` postfixOps = shuntHelper stack ts (output ++ [Operator op])
shuntHelper stack ((Operator LeftBracket) : ts) output = shuntHelper ((Operator LeftBracket) : stack) ts output
shuntHelper stack ((Operator RightBracket) : ts) output = do
    -- pop elements off the stack untill we find a matching opening bracket
    (l, r) <- maybeToEither "missing opening bracket" (splitOnce (Operator LeftBracket) stack)
    shuntHelper r ts (output ++ l)
shuntHelper stack ((Operator op) : ts) output = result
  where
    -- pop elements off the stack untill we find an operator with a lower binding power
    (opsToPush, newStack) = takeWhile (>= Operator op) stack
    newOutput = output ++ opsToPush
    result = shuntHelper ((Operator op) : newStack) ts newOutput
-- literals go straight to the output
shuntHelper stack ((Literal value) : ts) output = shuntHelper (stack) ts $ output ++ [Literal value]

-- >>> splitOnce (Operator LeftBracket) [Literal 1, Operator LeftBracket, Literal 2]
-- Just ([Literal 1.0],[Literal 2.0])

-- >>> shunt $ getRight $ lex "1 + 1 * 2"
-- Right [Literal 1.0,Literal 1.0,Literal 2.0,Operator Mult,Operator Add]

-- >>> getRight $ lex "(5)("
-- [Operator LeftBracket,Literal 5.0,Operator RightBracket,Operator LeftBracket]

-- >>> shunt $ getRight $ lex "1+(-5)+1"
-- Right [Literal 1.0,Literal 5.0,Operator Sub,Operator Add,Literal 1.0,Operator Add]

-- >>> shunt $ getRight $ lex "(1 +2) * 3"
-- Right [Literal 1.0,Literal 2.0,Operator Add,Literal 3.0,Operator Mult,Operator LeftBracket]

-- >>>shunt [Literal 1.0, Operator Add]

-- >>> takeWhile (>= Operator Mult) [Operator Add]
-- ([],[Operator Add])

-- put values from list into fst while predicate holds then place remaining in snd
takeWhile :: (Ord a) => (a -> Bool) -> [a] -> ([a], [a])
takeWhile p [] = ([], [])
takeWhile p (x : xs)
    | p x = (\(l, r) -> (x : l, r)) $ takeWhile p xs
    | otherwise = ([], x : xs)

-- >>> takeWhile (<= 1) [1,2]
-- ([1],[2])

-- split list on first occurence of deliminator (exclude it from output)
splitOnce :: (Eq a) => a -> [a] -> Maybe ([a], [a])
splitOnce deliminator (top : base)
    | top == deliminator = Just ([], base) -- freaky ahh fmap
    | otherwise = (\(l, r) -> (top : l, r)) <$> splitOnce deliminator base
splitOnce deliminator [] = Nothing

-- >>> splitOnce '=' "this=that"
-- Just ("this","that")

-- reverse polish notation is the goatttttttt
eval :: [Token] -> Either String Double
eval ts = evalHelper [] ts

-- >>> eval $ getRight $ lex "1 2 + 3 4 + *"
-- Right 21.0

getRight :: Either a b -> b
getRight (Right b) = b

-- stack based postfix expression evaluator
evalHelper :: [Double] -> [Token] -> Either String Double
-- base cases
evalHelper [result] [] = Right result
evalHelper stack [] = Left "result of calculation did not yield 1 item on the stack"
-- stack evaluating
evalHelper stack ((Literal x) : ts) = evalHelper (x : stack) ts
evalHelper stack ((Operator op) : ts) = case applyOperator op stack of
    Left e -> Left e
    Right newStack -> evalHelper newStack ts

getBinaryOperatorFunc :: Operator -> Double -> Double -> Double
getBinaryOperatorFunc Add = (+)
getBinaryOperatorFunc Sub = (-)
getBinaryOperatorFunc Mult = (*)
getBinaryOperatorFunc Div = (/)
getBinaryOperatorFunc Pow = (**) -- this one supports fractional exponents

getUnaryOperatorFunc :: Operator -> Double -> Double
getUnaryOperatorFunc Fact = factorial
getUnaryOperatorFunc Sin = sin
getUnaryOperatorFunc Cos = cos
getUnaryOperatorFunc Tan = tan

factorial :: Double -> Double
factorial 0 = 1
factorial n = n * (factorial $ n - 1)

applyOperator :: Operator -> [Double] -> Either String [Double]
-- unary ops require special treatment
applyOperator operator (xs)
    | operator `elem` unaryOps = applyUnaryOperator operator xs
-- sub with one operand is making a number negative
applyOperator Sub [first] = Right [0 - first]
-- implicit 0 or 1 for operators without both operands
applyOperator Add [first] = Right $ first : []
applyOperator operator [first]
    | operator `elem` binaryOps = Right $ ((getBinaryOperatorFunc operator) first 1) : []
-- propper applicatino with 2 opperands
applyOperator operator (first : second : others)
    | operator `elem` binaryOps = Right $ ((getBinaryOperatorFunc operator) second first) : others
applyOperator operator _ = Left "stack empty when applying operator"

applyUnaryOperator :: Operator -> [Double] -> Either String [Double]
applyUnaryOperator Fact (x : xs) = Right $ factorial x : xs
applyUnaryOperator _ _ = Left "stack empty when applying operator"

-- >>> eval $ getRight . shunt . getRight $ lex "1 - 2"
-- Right (-1.0)

-- >>> shunt . getRight $ lex "1 - 2"
-- Right [Literal 1.0,Literal 2.0,Operator Sub]

-- >>> getRight . shunt . getRight $ lex "5*(-3)"
-- [Literal 5.0,Literal 3.0,Operator Sub,Operator Mult]

-- >>> (-) 2 1
-- 1
