module Main where

import Numeric
import Control.Monad
import Control.Monad.Error
import Data.Char
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool
             | Float Double

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal val = case val of
  (String contents) -> "\"" ++ contents ++ "\""
  (Atom name) -> name
  (Number contents) -> show contents
  (Bool True) -> "#t"
  (Bool False) -> "#f"
  (List contents) -> "(" ++ unwordsList contents ++ ")"
  (DottedList head tail) -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
  (Character c) -> "#/" ++ [c]
  (Float f) -> show f

instance Show LispError where
  show = showErr

showErr :: LispError -> String
showErr err = case err of
  (UnboundVar message varname)  -> message ++ ": " ++ varname
  (BadSpecialForm message form) -> message ++ ": " ++ show form
  (NotFunction message func)    -> message ++ ": " ++ show func
  (NumArgs expected found)      -> "Expected " ++ show expected
                                         ++ " args; found values " ++ unwordsList found
  (TypeMismatch expected found) -> "Invalid type: expected " ++ expected
                                         ++ ", found " ++ show found
  (Parser parseErr)             -> "Parse error at " ++ show parseErr

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedCharacters :: Parser Char
escapedCharacters = do
  char '\\'
  c <- oneOf "\\\"nrt"
  return $ case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> c

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ (noneOf "\"") <|> escapedCharacters
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return (Atom $ first:rest)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = liftM (Number . read) $ many1 digit
-- fyi, liftM :: (a -> b) -> m a -> m b

-- parseDecimal1 = do
--   numString <- many1 digit
--   return $ (Number . read) numString

-- parseDecimal1 = (many1 digit) >>= (\digits ->
--               return $ (Number . read) digits)

parseDecimal2 :: Parser LispVal
parseDecimal2 = try $ do
  string "#d"
  num <- many1 digit
  return $ Number (read num)

hex2digit = fst . head . readHex

parseHex :: Parser LispVal
parseHex = try $ do
  string "#x"
  num <- many1 hexDigit
  return $ Number (hex2digit num)

oct2digit = fst . head . readOct

parseOct :: Parser LispVal
parseOct = try $ do
  string "#o"
  num <- many1 octDigit
  return $ Number (oct2digit num)

bin2digit = foldl (\acc item -> (2 * acc) + (toInteger $ digitToInt item)) 0

parseBin :: Parser LispVal
parseBin = try $ do
  string "#b"
  num <- many1 (oneOf "01")
  return $ Number (bin2digit num)

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  c <- anyChar
  return $ Character c

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $ readFloat (x ++ "." ++ y))

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
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat
        <|> try parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List ((Atom fn) : args)) = mapM eval args >>= apply fn
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function args" fn)
                      ($ args)
                      (lookup fn primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("number?", unaryOp numberp),
              ("boolean?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("string?", unaryOp stringp),
              ("symbol->string", unaryOp symToStr),
              ("string->symbol", unaryOp strToSym)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f args = case args of
  []    -> throwError $ NumArgs 1 args
  (x:_) -> return $ f x

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op args
  | (length args) < 2 = throwError $ NumArgs 2 args
  | otherwise = mapM unpackNum args >>= (return . Number . foldl1 op)

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symToStr :: LispVal -> LispVal
symToStr (Atom x) = String $ map toLower x
symToStr _ = String ""

strToSym :: LispVal -> LispVal
strToSym (String x) = Atom x
strToSym _ = Atom ""

unpackNum :: LispVal -> ThrowsError Integer
unpackNum item = case item of
  (Number n) -> return n
  (String s) -> let parsed = reads s :: [(Integer, String)]
                in if null parsed
                  then throwError $ TypeMismatch "number" $ String s
                  else return $ (fst . head) parsed
  (List [n]) -> unpackNum n
  otherwise  -> throwError $ TypeMismatch "number" item

main :: IO ()
main = do
  (expr:_) <- getArgs
  evaled <- return $ liftM show $ readExpr expr >>= eval
  (putStrLn . extractValue . trapError) evaled
