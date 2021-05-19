module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except

--- *** Types *** ---

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
             deriving (Show)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError


--- *** Utils *** ---
trapError action = catchError action ( return . show )
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


--- *** Parsers *** ---
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser()
spaces = skipMany1 space

parseString :: Parser LispVal
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
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do
--   x <- many1 digit
--   return $ (Number . read) x
parseNumber = many1 digit >>= (\x -> return $ ( Number . read ) x )


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr 
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted 
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x

--- *** Evaluator *** ---
-- Notice this has the output type of LispVal not just matched type 
eval :: LispVal -> ThrowsError LispVal 
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("not", unaryOp not'),
              ("boolean?", unaryOp boolP),
              ("list?", unaryOp listP),
              ("symbol?", unaryOp symbolP),
              ("string?", unaryOp stringP),
              ("number?", unaryOp numberP)
             ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp func []  = throwError $ NumArgs 2 []
unaryOp func [val] = return $ func val

not' (Bool x) = (Bool . not) x
not' _ = Bool False

boolP (Bool _) = Bool True
boolP _ = Bool False


numberP (Number _) = Bool True
numberP _ = Bool False


listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False

symbolP (Atom _) = Bool True
symbolP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


--- *** Executor *** ---
readExpr :: String -> ThrowsError LispVal 
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err 
    Right val -> return val

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled






















