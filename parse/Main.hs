module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment 
import Control.Monad
import Numeric

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
                         
--  parseNumber :: Parser LispVal
--  parseNumber = liftM (Number . read) $ many1 digit 
 
parseHex :: Parser LispVal
parseHex = do
            char '#'
            char 'x'
            digits <- many1 hexDigit
            return $ Number $ fst ( (readHex digits) !! 0 )
            
-- parse: user error (Pattern match failure in do expression at parse.hs:80:10-17)
-- Why?            
 
parseRawDec = do
            digits <- many1 digit
            return $ Number $ read digits
            
parseNumber1 :: Parser LispVal
parseNumber1 = parseHex
            <|> parseRawDec


--  parseNumber2 :: Parser LispVal
--  parseNumber2 = many1 digit >>= \p -> return . (Number . read) $ p
                          
parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseNumber1
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