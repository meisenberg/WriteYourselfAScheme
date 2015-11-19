module TestSafePrelude where

import Test.HUnit
import Main hiding (main)

testParseString :: Test
testParseString = 
    TestCase $ assertEqual "Should parse a string with escaped slash character"
                           ("Found value: " ++ show (String "ab\\c"))  (parserRunner parseString "lisp" "\"ab\\\\c\"")

testParseNum :: Test
testParseNum = 
    TestCase $ assertEqual "Should parse a decimal number with no prefix"
                           ("Found value: " ++ show (Number 123))  (parserRunner parseNumber "lisp" "123")

testParseNumHex :: Test
testParseNumHex = 
    TestCase $ assertEqual "Should parse a hexadecimal number with prefix"
                           ("Found value: " ++ show (Number 291))  (parserRunner parseNumber "lisp" "#x123")

testParseNumOct :: Test
testParseNumOct = 
    TestCase $ assertEqual "Should parse a octal number with prefix"
                           ("Found value: " ++ show (Number 83))  (parserRunner parseNumber "lisp" "#o123")

testParseNumBinary :: Test
testParseNumBinary = 
    TestCase $ assertEqual "Should parse a binary number with prefix"
                           ("Found value: " ++ show (Number 5))  (parserRunner parseNumber "lisp" "#b101")

main :: IO Counts
main = runTestTT $ TestList [testParseString, testParseNum, testParseNumHex, testParseNumOct, testParseNumBinary]


