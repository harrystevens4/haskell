module Main where
import System.Environment as SysEnv
import Text.Read hiding (lex)
import Prelude hiding (lex,takeWhile)
import System.Exit
import Data.Char

data Token = Literal Double | Operator Operator deriving (Show,Eq)
data Operator = 
      Add         | Sub
    | Div         | Mult
    | Pow
    | LeftBracket | RightBracket deriving (Show,Eq)

main = do
    args <- SysEnv.getArgs
    case args of
        [arg1] -> calculateThenPrint arg1
        [] -> getContents >>= calculateThenPrint
        _ -> putStrLn "Usage: csfc \"expression\""

calculateThenPrint :: String -> IO ()
calculateThenPrint expr = case calculate expr of
            Right result -> print result >> exitSuccess
            Left e -> putStrLn e >> exitFailure


calculate :: String -> Either String Double
calculate expr = lex expr >>= shunt >>= eval

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
    | isNumber c = case (reads (c:cs) :: [(Double,String)]) of
        -- add a space so that any minus sign directly following a literal is treated as an operator
        [(x,'-':unparsed)] -> (\ts -> Literal x : ts) <$> (lex $ '-':' ':unparsed)
        [(x,unparsed)] -> (\ts -> Literal x : ts) <$> (lex unparsed)
        _ -> Left "error parsing double"
-- parse brackets
lex ('(':cs) = (\xs -> (Operator LeftBracket):xs) <$> (lex cs)
lex (')':cs) = (\xs -> (Operator RightBracket):xs) <$> (lex cs)
-- parse an operator
lex (c:cs) = parseOperator c >>= \op -> (\x -> (Operator op) : x) `fmap` (lex cs)

-- >>> getRight . lex $ "10*(-4)"
-- [Literal 10.0,Operator LeftBracket,Literal (-4.0),Operator RightBracket]

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e (Just x)  = Right x
maybeToEither e (Nothing) = Left e

-- >>> lex "1 2 +"
-- Right [Literal 1.0,Literal 2.0,Operator Add]

-- >>> (reads "1.1") :: [(Double,String)]
-- [(1.1,"")]

parseOperator :: Char -> Either String Operator
parseOperator '+' = Right Add
parseOperator '-' = Right Sub
parseOperator '/' = Right Div
parseOperator '*' = Right Mult
parseOperator '^' = Right Pow
parseOperator unparsable = Left $ "could not parse operator from \"" ++ [unparsable] ++ "\""

operatorBindingPower :: Operator -> Int
operatorBindingPower Add = 1
operatorBindingPower Sub = 1
operatorBindingPower Mult = 2
operatorBindingPower Div = 2
operatorBindingPower Pow = 3
operatorBindingPower _ = 0

instance Ord Token where
    Literal a >= Literal b = a >= b
    Operator a >= Operator b = a >= b
    Literal a <= Literal b = a <= b
    Operator a <= Operator b = a <= b
instance Ord Operator where
    x >= y = operatorBindingPower x >= operatorBindingPower y
    x <= y = operatorBindingPower x <= operatorBindingPower y

shunt :: [Token] -> Either String [Token]
shunt ts = shuntHelper [] ts []

--              stack      input      output
shuntHelper :: [Token] -> [Token] -> [Token] -> Either String [Token]
shuntHelper stack [] output = Right $ output ++ stack
shuntHelper stack ((Operator LeftBracket):ts) output = shuntHelper ((Operator LeftBracket):stack) ts output
shuntHelper stack ((Operator RightBracket):ts) output = do
    (l,r) <- maybeToEither "missing opening bracket" (splitOnce (Operator LeftBracket) stack)
    shuntHelper r ts (output ++ l)
shuntHelper stack ((Operator op):ts) output = result where
    (opsToPush,newStack) = takeWhile (>= Operator op) stack
    newOutput = output ++ opsToPush
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
    | otherwise          = (\(l,r) -> (top:l,r)) <$> splitOnce deliminator base
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
getOperatorFunc Pow = (**)

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
