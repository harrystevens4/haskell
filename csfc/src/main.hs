module Main where
import System.Environment as SysEnv
import Text.Read hiding (lex)
import Prelude hiding (lex)

data Token = Literal Double | Operator Operator deriving Show
data Operator = Add | Sub | Div | Mult deriving Show

main = do
    args <- SysEnv.getArgs
    case args of
        [arg1] -> print $ calculate arg1
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

shunt :: [Token] -> Either String [Token]
shunt ts = Right ts

eval :: [Token] -> Either String Double
eval ts = Right 0.0
