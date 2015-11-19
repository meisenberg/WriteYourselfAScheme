module TestSafePrelude where

import Test.HUnit
import Main hiding (main)

testParseAtom :: Test
testParseAtom = 
    TestCase $ assertEqual "Should parse a simple atom"
                           ("Found value: " ++ show (Atom "abc"))  (parserRunner parseAtom "lisp" "abc")

testParseAtomFail :: Test
testParseAtomFail = 
    TestCase $ assertEqual "Should parse a single '#' character as an Atom"
                           ("Found value: " ++ show (Atom "#"))  (parserRunner parseAtom "lisp" "#")

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

testParseSimpleCharacter :: Test
testParseSimpleCharacter = 
    TestCase $ assertEqual "Should parse a simple character object"
                           ("Found value: " ++ show (Character 'a'))  (parserRunner parseCharacter "lisp" "#\\a")

testParseCharacterName :: Test
testParseCharacterName = 
    TestCase $ assertEqual "Should parse a simple character object"
                           ("Found value: " ++ show (Character ' '))  (parserRunner parseCharacter "lisp" "#\\space")

main :: IO Counts
main = runTestTT $ TestList [testParseAtom, testParseAtomFail,
        testParseString, testParseNum, testParseNumHex, testParseNumOct, testParseNumBinary,
        testParseSimpleCharacter, testParseCharacterName]


