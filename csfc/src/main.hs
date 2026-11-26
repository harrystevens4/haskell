{-# LANGUAGE LambdaCase #-}
-- >>> (\Add -> Sub) Add
-- Sub
-- binaryOps :: (Operator, Operator, Operator, Operator, Operator)
-- evaluate the expression then print the result or error
-- such sexy syntax
-- im up in the club parsing it and by it i mean my lexemes
-- ignore whitespace
-- apply subtract as a negative e.g. - 5 * 10 is (-5) * 10
-- >>> lex "(1+3) * (4/2)" >>= shunt
-- Left "missing opening bracket"
-- >>> parseInput "(1+2+3)" (parseOperator <||> parseConstant)
-- Right (Operator LeftBracket,"1+2+3)")
-- >>> getRight . lex $ "(1-4)"
-- [Operator LeftBracket,Literal 1.0,Operator Sub,Literal 4.0,Operator RightBracket]
-- cant find in standard library?
-- >>> lex "1 2 +"
-- Right [Literal 1.0,Literal 2.0,Operator Add]
-- >>> (reads "1.1") :: [(Double,String)]
-- [(1.1,"")]
-- higher binding power is higher order of operations precidence
{-# OPTIONS_GHC -W #-}

module Main where

import Control.Monad
import Data.List hiding (takeWhile)
import Data.Maybe
import System.Environment as SysEnv
import System.Exit
import System.IO
import Text.Read hiding (get, lex)
import Prelude hiding (lex, takeWhile)

data Token = Literal Double | Operator Operator deriving (Show, Eq)

data ShuntState = ShuntState
    { stack :: [Operator]
    , output :: [Token]
    }

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
eitherOr (Left _) (b) = b

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
        Right (r, _) -> parseInput x (g r)

parseInput :: String -> Parser a -> Either String (a, String)
parseInput s (Parser f) = f s

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

operatorMap =
    [ (Add, "+")
    , (Sub, "-")
    , (Mult, "*")
    , (Div, "/")
    , (Fact, "!")
    , (Sin, "sin")
    , (Cos, "cos")
    , (Tan, "tan")
    , (Pow, "^")
    , (LeftBracket, "(")
    , (RightBracket, ")")
    ]

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

binaryOps =
    [ Add
    , Sub
    , Div
    , Mult
    , Pow
    ]

flipTuples :: [(a, b)] -> [(b, a)]
flipTuples = fmap (uncurry $ flip (,))

startsWith :: String -> String -> Bool
startsWith a b = foldr (\(a, b) x -> x && a == b) True (zip a b)

greedyMatch :: String -> String -> (Bool, String)
greedyMatch (a : as) (b : bs)
    | a == b = greedyMatch as bs
    | otherwise = (False, b : bs)
greedyMatch [] b = (True, b)
greedyMatch _ [] = (False, "")

-- >>> greedyMatch "abc" "aabcdef"
-- (False,"abcdef")

main = do
    args <- SysEnv.getArgs
    case args of
        [arg1] -> calculateThenPrint arg1
        [] -> getContents >>= calculateThenPrint
        _ -> putStrLn "Usage: csfc \"expression\""

calculateThenPrint :: String -> IO ()
calculateThenPrint expr = case calculate expr of
    Right result -> print result >> exitSuccess
    Left e -> hPutStrLn stderr e >> exitFailure

calculate :: String -> Either String Double
calculate expr = lex expr >>= shunt >>= eval

parseConstant :: Parser Token
parseConstant = fmap Literal $ Parser $ \case
    ('p' : 'i' : cs) -> Right (pi, cs)
    cs -> Left $ "unknown constant " ++ show cs

parseOperator :: Parser Token
parseOperator = Parser $
    \x ->
        maybeToEither "unknown operator" $
            listToMaybe
                ( do
                    (op, str) <- operatorMap
                    case greedyMatch str x of
                        (True, xs) -> [(Operator op, xs)]
                        (False, _) -> []
                )

parseLiteral :: Parser Token
parseLiteral = fmap Literal $ Parser $ \x -> case reads x of
    (x : _) -> Right x
    [] -> Left $ "unknown literal " ++ show x

lex :: String -> Either String [Token]
lex [] = Right []
lex (' ' : cs) = lex cs
lex ('\n' : cs) = lex cs
lex ('\r' : cs) = lex cs
lex ('\t' : cs) = lex cs
lex cs = do
    (op, remaining) <-
        parseInput
            cs
            ( parseConstant
                <||> parseLiteral
                <||> parseOperator
            )
    ops <- lex remaining
    pure . applyNegative $ (op : ops)

applyNegative :: [Token] -> [Token]
applyNegative (Operator a : Operator Sub : Literal c : ts)
    | a /= RightBracket = Operator a : Literal (0 - c) : applyNegative ts
applyNegative (t : ts) = t : applyNegative ts
applyNegative [] = []

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither e (Nothing) = Left e

operatorBindingPower :: Operator -> Int
operatorBindingPower Add = 1
operatorBindingPower Sub = 1
operatorBindingPower Mult = 2
operatorBindingPower Div = 2
operatorBindingPower Pow = 3
operatorBindingPower Fact = 3
operatorBindingPower x
    | x `elem` prefixOps = 10
    | x `elem` [LeftBracket, RightBracket] = 0
    | otherwise = undefined

-- >>> LeftBracket `elem` [LeftBracket,RightBracket]
-- True

-- compare by binding power
instance Ord Token where
    Literal a >= Literal b = a >= b
    Operator a >= Operator b = a >= b
    Literal a <= Literal b = a <= b
    Operator a <= Operator b = a <= b

instance Ord Operator where
    x >= y = operatorBindingPower x >= operatorBindingPower y
    x <= y = operatorBindingPower x <= operatorBindingPower y

showTokens :: [Token] -> String
showTokens = unwords . (fmap showToken)
showToken (Literal x) = show x
showToken (Operator x) = (\(Just a) -> a) $ lookup x operatorMap

shunt :: [Token] -> Either String [Token]
shunt ts = do
    (ShuntState stack output) <-
        foldl
            ( \acc (t, i) -> do
                state <- acc
                case shuntToken t state of
                    Left e -> Left $ showTokens (take i ts) ++ " <- failed to shunt here: " ++ e
                    Right x -> Right x
            )
            (Right $ ShuntState [] [])
            (zip ts [1 ..])
    case findIndex (== LeftBracket) stack of
        Nothing -> Right $ output ++ (Operator <$> stack)
        Just i -> Left $ showTokens (take (1 + (findIndices (== Operator LeftBracket) ts !! i)) ts) ++ " <- failed to shunt here: mismatched opening bracket"

-- >>> :t (ShuntState [], [], Right [])
-- (ShuntState [], [], Right [])
--   :: ([Token] -> Either String [Token] -> ShuntState, [a1],
--       Either a2 [a3])

sMapStack :: ([Operator] -> [Operator]) -> ShuntState -> ShuntState
sMapStack f s = s{stack = f (stack s)}

sMapOutput :: ([Token] -> [Token]) -> ShuntState -> ShuntState
sMapOutput f s = s{output = f (output s)}

shuntToken :: Token -> ShuntState -> Either String ShuntState
shuntToken (Operator LeftBracket) s = Right . sMapStack (LeftBracket :) $ s
shuntToken (Operator RightBracket) s = do
    (newOutput, newStack) <-
        maybeToEither
            "missing opening bracket"
            (splitOnce LeftBracket (stack s))
    pure . (sMapStack $ const newStack) . (sMapOutput (++ (Operator <$> newOutput))) $ s
shuntToken (Operator op) s = do
    let (opsToPush, newStack) = takeWhile (>= op) (stack s)
    pure . (sMapStack $ const (op : newStack)) . (sMapOutput (++ (Operator <$> opsToPush))) $ s
shuntToken (Literal value) s = Right $ sMapOutput (++ [Literal value]) s

-- shunting yard algorithm to convert infix into postfix
--              stack      input      output
-- shuntHelper :: [Token] -> [Token] -> [Token] -> Either String [Token]
-- shuntHelper stack [] output = Right $ output ++ stack
-- shuntHelper stack ((Operator LeftBracket) : ts) output = shuntHelper ((Operator LeftBracket) : stack) ts output
-- shuntHelper stack ((Operator RightBracket) : ts) output = do
--     -- pop elements off the stack untill we find a matching opening bracket
--     (l, r) <- maybeToEither ("missing opening bracket: stack is " ++ show stack ++ " output is " ++ show output) (splitOnce (Operator LeftBracket) stack)
--     shuntHelper r ts (output ++ l)
-- -- pop stack onto output once we run out of input tokens
-- -- shuntHelper stack (Operator op : ts) output
-- -- \| op `elem` postfixOps = shuntHelper stack ts (output ++ [Operator op])
-- -- \| otherwise = shuntHelper stack ts (output ++ [Operator op])
-- shuntHelper stack ((Operator op) : ts) output = result
--   where
--     -- pop elements off the stack untill we find an operator with a lower binding power
--     (opsToPush, newStack) = takeWhile (>= Operator op) stack
--     newOutput = output ++ opsToPush
--     result = shuntHelper ((Operator op) : newStack) ts newOutput
-- -- literals go straight to the output
-- shuntHelper stack ((Literal value) : ts) output = shuntHelper (stack) ts $ output ++ [Literal value]

-- >>> takeWhile (< 5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])

-- >>> splitOnce (Operator LeftBracket) [Literal 1, Operator LeftBracket, Literal 2]
-- Just ([Literal 1.0],[Literal 2.0])

-- >>> shunt $ getRight $ lex "+"
-- Right []

-- >>> shunt $ getRight $ lex "(1+3) * (4/2)"
-- Right [Operator LeftBracket]

-- >>> getRight $ lex "(5)("
-- [Operator LeftBracket,Literal 5.0,Operator RightBracket,Operator LeftBracket]

-- >>> lex "1+(-5)+1"
-- Right [Literal 1.0,Operator Add,Operator LeftBracket,Operator Sub,Literal 5.0,Operator RightBracket,Operator Add,Literal 1.0]

-- >>> shunt [Literal 1.0,Operator Add,Operator LeftBracket,Operator Sub,Literal 5.0,Operator RightBracket,Operator Add,Literal 1.0]
-- Left "missing opening bracket: stack is [Operator Sub] output is [Literal 1.0,Operator LeftBracket,Operator Add,Literal 5.0]"

-- >>> shunt [Operator Add,Operator LeftBracket, Operator Add, Literal 5.0, Operator RightBracket, Operator LeftBracket, Operator RightBracket, Literal 5.0]
-- Left "missing opening bracket"

-- >>> lex "(1 +2) * 3"
-- Right [Operator LeftBracket,Literal 1.0,Operator Add,Literal 2.0,Operator RightBracket,Operator Mult,Literal 3.0]

-- >>>shunt [Literal 1.0, Operator Add]

-- >>> takeWhile (>= Operator Mult) [Operator Add   ]
-- ([],[Operator Add])

-- put values from list into fst while predicate holds then place remaining in snd
takeWhile :: (Ord a) => (a -> Bool) -> [a] -> ([a], [a])
takeWhile _ [] = ([], [])
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
splitOnce _ [] = Nothing

-- >>> splitOnce '=' "this=that"
-- Just ("this","that")

-- reverse polish notation is the goatttttttt
eval :: [Token] -> Either String Double
eval ts = evalHelper [] ts

-- >>> eval $ getRight $ lex "1 2 + 3 4 + *"
-- Right 21.0

getRight :: Either a b -> b
getRight (Right b) = b
getRight _ = undefined

-- stack based postfix expression evaluator
evalHelper :: [Double] -> [Token] -> Either String Double
-- base cases
evalHelper [result] [] = Right result
evalHelper _ [] = Left "result of calculation did not yield 1 item on the stack"
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
getBinaryOperatorFunc _ = undefined

getUnaryOperatorFunc :: Operator -> Double -> Double
getUnaryOperatorFunc Fact = factorial
getUnaryOperatorFunc Sin = roundToWhole . sin
getUnaryOperatorFunc Cos = roundToWhole . cos
getUnaryOperatorFunc Tan = roundToWhole . tan
getUnaryOperatorFunc _ = undefined

-- >>> eval $ getRight $ lex "0 sin"
-- Left "stack empty when applying operator"

epsilon = 1e-15

roundToWhole :: Double -> Double
roundToWhole x =
    if (abs $ (fromIntegral . round $ x) - x) <= epsilon
        then fromIntegral . round $ x
        else x

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
-- propper application with 2 opperands
applyOperator operator (first : second : others)
    | operator `elem` binaryOps = Right $ ((getBinaryOperatorFunc operator) second first) : others
applyOperator _ _ = Left "stack empty when applying operator"

applyUnaryOperator :: Operator -> [Double] -> Either String [Double]
applyUnaryOperator op (x : xs) = Right $ getUnaryOperatorFunc op x : xs
applyUnaryOperator _ _ = Left "stack empty when applying operator"

-- >>> eval $ getRight . shunt . getRight $ lex "1 - 2"
-- Right (-1.0)

-- >>> shunt . getRight $ lex "1 - 2"
-- Right [Literal 1.0,Literal 2.0,Operator Sub]

-- >>> getRight . shunt . getRight $ lex "5*(-3)"
-- [Literal 5.0,Literal 3.0,Operator Sub,Operator Mult]

-- >>> (-) 2 1
-- 1
