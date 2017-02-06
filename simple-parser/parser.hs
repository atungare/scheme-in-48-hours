module Main where

import Numeric
import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool
             | Float Double
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
  show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show = showErr

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows eith = case eith of
  (Left err) -> throwError err
  (Right val) -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                 (liftIO . (flip writeIORef val))
                                 (lookup var env)
                           return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do previouslyDefined <- liftIO $ isBound envRef var
                              if previouslyDefined
                                then setVar envRef var val >> return val
                                else liftIO $ do valRef <- newIORef val
                                                 env <- readIORef envRef
                                                 writeIORef envRef ((var, valRef) : env)
                                                 return val


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef kvPairs = readIORef envRef >>= extendEnv kvPairs >>= newIORef
  where
    extendEnv pairs env = liftM (++ env) (mapM addPairs pairs)
    addPairs (key, val) = do ref <- newIORef val
                             return (key, ref)

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
  (PrimitiveFunc _) -> "<primitive>"
  (Func { params = args, vararg = varargs, body = body, closure = env})
    -> "(lambda (" ++ unwords (map show args) ++ case varargs of
                                                   Nothing -> ""
                                                   (Just arg) ->  " . " ++ arg
                                              ++ ") ...)"
  (Port _) -> "<IO port>"
  (IOFunc _) -> "<IO primitive>"

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env expression = case expression of
  val@(String _)                                    -> return val
  val@(Number _)                                    -> return val
  val@(Bool _)                                      -> return val
  (Atom x)                                          -> getVar env x
  (List [Atom "load", String filename]) -> load filename >>= liftM last . mapM (eval env)
  (List [Atom "quote", val])                        -> return val
  (List [Atom "if", predicate, thenExpr, elseExpr]) -> do test <- eval env predicate
                                                          case test of
                                                            (Bool False) -> eval env elseExpr
                                                            _            -> eval env thenExpr
  (List [Atom "set!", Atom var, form])              -> eval env form >>= setVar env var
  (List [Atom "define", Atom var, form])            -> eval env form >>= defineVar env var
  (List (Atom "define" : List (Atom var : params) : body))
    -> makeNormalFunc env params body >>= defineVar env var
  (List (Atom "define" : DottedList (Atom var : params) varargs : body))
    -> makeVarArgs varargs env params body >>= defineVar env var
  (List (Atom "lambda" : List params : body))
    -> makeNormalFunc env params body
  (List (Atom "lambda" : DottedList params varargs : body))
    -> makeVarArgs varargs env params body
  (List (Atom "lambda" : varargs@(Atom _) : body))
    -> makeVarArgs varargs env [] body
  (List (fn : args))
    -> do func <- eval env fn
          argVals <- mapM (eval env) args
          apply func argVals
  badForm                                           -> throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply fn args = case fn of
  (PrimitiveFunc func) -> liftThrows $ func args
  (Func params varargs body closure) ->
    if num params /= num args && varargs == Nothing
      then throwError $ NumArgs (num params) args
      else (liftIO $ bindVars closure $ zip params args) >>=
           bindVarArgs varargs >>=
           evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs vargs env = case vargs of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
  (IOFunc func) -> func args
  _ -> throwError $ NotFunction "not a function" (unwords $ map showVal args)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFn PrimitiveFunc) primitives
                                                 ++ map (makeFn IOFunc) ioPrimitives)
  where makeFn constructor (var, func) = (var, constructor func)

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
              ("string->symbol", unaryOp strToSym),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker test args = case args of
  x:y:[]    -> do a <- unpacker x
                  b <- unpacker y
                  return $ Bool $ test a b
  otherwise -> throwError $ NumArgs 2 args

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f args = case args of
  x:[]      -> return $ f x
  otherwise -> throwError $ NumArgs 1 args

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op args
  | length args < 2 = throwError $ NumArgs 2 args
  | otherwise       = mapM unpackNum args >>= (return . Number . foldl1 op)

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
  notNum  -> throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr item = case item of
  (String s) -> return s
  (Number s) -> return $ show s
  (Bool s)   -> return $ show s
  notString  -> throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool item = case item of
  (Bool b) -> return b
  notBool  -> throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car args = case args of
  [(List (x:_))]         -> return x
  [(DottedList (x:_) _)] -> return x
  [badArg]               -> throwError $ TypeMismatch "pair" badArg
  badArgList             -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr args = case args of
  [(List (_:xs))]         -> return $ List xs
  [(DottedList (_:[]) x)] -> return x
  [(DottedList (_:xs) x)] -> return $ DottedList xs x
  [badArg]                -> throwError $ TypeMismatch "pair" badArg
  badArgList              -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons args = case args of
  [x, (List xs)]          -> return $ List $ (x:xs)
  [x, DottedList xs last] -> return $ DottedList (x : xs) last
  [x, y]                  -> return $ DottedList [x] y
  badArgList              -> throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv args = case args of
  [(Bool arg1), (Bool arg2)]             -> return $ Bool $ arg1 == arg2
  [(Number arg1), (Number arg2)]         -> return $ Bool $ arg1 == arg2
  [(String arg1), (String arg2)]         -> return $ Bool $ arg1 == arg2
  [(Atom arg1), (Atom arg2)]             -> return $ Bool $ arg1 == arg2
  [(DottedList xs x), (DottedList ys y)] -> eqv [List $ xs ++ [x], List $ ys ++ [y]]
  [(List arg1), (List arg2)]             -> return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
  [_, _]                                 -> return $ Bool False
  badArgList                             -> throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) = catchError
  (do unpackedX <- unpacker x
      unpackedY <- unpacker y
      return $ unpackedX == unpackedY)
  (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal args = case args of
  [x, y]     -> do
                  (Bool equiv) <- eqv [x, y]
                  unpackEq <- liftM or $ mapM (unpackEquals x y) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                  return $ Bool (equiv || unpackEq)
  badArgList -> throwError $ NumArgs 2 badArgList

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ test prompt action =
  do
    result <- prompt
    if test result then return ()
                   else action result >> until_ test prompt action

runOne :: [String] -> IO ()
runOne expr = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 expr)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (expr !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do env <- primitiveBindings
             until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint env)

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
