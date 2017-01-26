module Main where

import Numeric
import Control.Monad
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
  return (Atom  $ first:rest)

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
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat
        <|> try parseNumber
        <|> parseString
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found Value: " ++ show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
