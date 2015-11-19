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
        deriving (Show)
             
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

                          
parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseNumber
        <|> parseString
 
parserRunner :: Show a => Parser a -> String -> String -> String
parserRunner parser label input = case parse parser label input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value: " ++ show val 
     
readExpr :: String -> String
readExpr input = parserRunner parseExpr "lisp" input
                              
main :: IO ()
main = do 
        (expr:_) <- getArgs
        putStrLn (readExpr expr)