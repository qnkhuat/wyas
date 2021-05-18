module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Contorl.Monad

-- *** Parsers
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

parseString :: Parser Lispval
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _ -> Atom atom

-- $ is function application 
-- . is function composition
-- we have the liftM because $ many1 digit is going to return a Parser String
-- which (Number . read) can't operate on so we use liftM to tell (Number . read)
-- to operates on the value inside Parser String returned by $ many1 digit
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber

-- *** Executor
readExpr :: String -> String
readExpr input = case parse parseExprt  "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)


-- This called algebraic data type. Defines a set of possible values that the
-- data type Lispval can hold
-- Each Atom, List is called a constructor
-- the 2nd argument define the type of this constructor
-- We can use this constructor as the function to arguments into a value of its type
-- Forexample String "abc" will turn "abc" into LispVal
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
