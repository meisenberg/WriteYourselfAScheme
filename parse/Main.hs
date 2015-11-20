module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment 
import Control.Monad
import Numeric
import Data.Char (digitToInt)


data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool
        | Character Char
             
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 

spaces :: Parser ()
spaces = skipMany1 space    

parseString :: Parser LispVal
parseString = do
        char '"'
        x <- many parseStringChar
        char '"'
        return $ String x

parseStringChar :: Parser Char
parseStringChar = (parseEscapedChar <|> noneOf "\"")

parseEscapedChar :: Parser Char
parseEscapedChar = do
        char '\\'
        x <- oneOf "nt\\\"" -- one of the following characters: n, t, \, "
        return $
            case x of
                'n' -> '\n'
                't' -> '\t'
                _ -> x -- it's either \ or " so we can return it "as is" 
                         
parseAtom :: Parser LispVal
parseAtom = do 
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ case atom of 
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _    -> Atom atom

arrayToString = \x -> concat(map(show)(x))
                         
binDigits = arrayToString [0, 1]
octalDigits = arrayToString [0..7]
decDigits = arrayToString [0..9]
hexDigits = decDigits ++ ['a'..'f']

parseNumPrefix :: Parser Integer
parseNumPrefix = do
            char '#'
            prefix <- oneOf "bodx"
            let validDigits = case prefix of 
                    'b' -> binDigits
                    'o' -> octalDigits
                    'd' -> decDigits
                    'x' -> hexDigits
            digits <- many1 (oneOf validDigits)
            return $ case prefix of
                        'b' -> fst $ (readInt 2 (`elem` "01") digitToInt digits) !! 0
                        'o' -> fst $ (readOct digits) !! 0
                        'd' -> read digits
                        'x' -> fst $ (readHex digits) !! 0
            
parseNum :: Parser Integer            
-- Use 'do' to lift Parser String to Parser Integer
parseNum = do   
            digits <- many1 digit
            return $ read digits
     
--  parseNumber :: Parser LispVal
--  parseNumber = liftM (Number . read) $ many1 digit  
            
parseNumber :: Parser LispVal
parseNumber = do 
        x <- parseNumPrefix <|> parseNum
        return (Number x)
                           
parseCharacter :: Parser LispVal
parseCharacter = parseLongChar <|> parseShortChar

parseShortChar :: Parser LispVal 
parseShortChar = do 
                 string "#\\"
                 x <- anyChar 
                 return $ Character x

parseLongChar :: Parser LispVal
parseLongChar = do 
                  string "#\\"
                  x <- (string "space" <|> string "newline")
                  case x of 
                   "space" -> return $ Character ' '
                   "newline" -> return $ Character '\n'     


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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- make the LispVal class an instance of the Show class
instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
                 
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) (\x -> x $ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", isString),
              ("number?", isNumber),
              ("symbol?", isSymbol)
              ]
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
              
isString :: [LispVal] -> LispVal
isString params = case params of
    [String x] -> Bool True 
    _ -> Bool False              

isNumber :: [LispVal] -> LispVal
isNumber params = case params of
    [Number x] -> Bool True 
    _ -> Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol params = case params of
    [Atom x] -> Bool True 
    _ -> Bool False                  
                                          
parserRunner :: Show a => Parser a -> String -> String -> String
parserRunner parser label input = case parse parser label input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value: " ++ show val 
     
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val
                                      
main :: IO ()
main = getArgs >>= print . eval . readExpr . head        