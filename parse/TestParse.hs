module TestSafePrelude where

import Test.HUnit
import Main hiding (main)

testParseString :: Test
testParseString = 
    TestCase $ assertEqual "Should parse a string with escaped slash character"
                           ("Found value: " ++ show (String "ab\\c"))  (parserRunner parseString "lisp" "\"ab\\\\c\"")

testParseNumRawDec :: Test
testParseNumRawDec = 
    TestCase $ assertEqual "Should parse a decimal number with no prefix"
                           ("Found value: " ++ show (Number 123))  (parserRunner parseRawDec "lisp" "123")

main :: IO Counts
main = runTestTT $ TestList [testParseString, testParseNumRawDec]


