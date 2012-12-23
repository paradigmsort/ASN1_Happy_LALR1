{
module ASN1Lexer where
import Test.HUnit
import BasicTypes
}

%wrapper "basic"

$digit = 0-9
$bindigit = 0-1
$hexdigit = [0-9A-F]
$nonzero = 1-9
$upper = [A-Z]
$lower = [a-z]
$alpha = [A-Za-z]

tokens :-
    $white                          ;
    :=                              { KeywordToken }
    \.\.\.                          { KeywordToken }
    \[\[                            { KeywordToken }
    \]\]                            { KeywordToken }
    \{                              { KeywordToken }
    \}                              { KeywordToken }
    \,                              { KeywordToken }
    \(                              { KeywordToken }
    \)                              { KeywordToken }
    \-                              { KeywordToken }
    :                               { KeywordToken }
    BIT                             { KeywordToken }
    BOOLEAN                         { KeywordToken }
    CHOICE                          { KeywordToken }
    DEFAULT                         { KeywordToken }
    ENUMERATED                      { KeywordToken }
    INTEGER                         { KeywordToken }
    OCTET                           { KeywordToken }
    OF                              { KeywordToken }
    OPTIONAL                        { KeywordToken }
    SEQUENCE                        { KeywordToken }
    STRING                          { KeywordToken }
    TRUE                            { KeywordToken }
    FALSE                           { KeywordToken }
    $upper (\- $alpha | $alpha )*   { TypeOrModuleReferenceToken }
    $lower (\- $alpha | $alpha )*   { IdentifierOrValueReferenceToken }
    0      | $nonzero $digit*       { NumberToken . read }
    \'( $bindigit | $white )*\'B    { BStringToken . toBString }
    \'( $hexdigit | $white )*\'H    { HStringToken . toHString }

{
data ASN1Token = TypeOrModuleReferenceToken String
               | IdentifierOrValueReferenceToken String
               | NumberToken Integer
               | BStringToken [Bit]
               | HStringToken [Hex]
               | KeywordToken String deriving (Show, Eq)

buildList :: (a -> Maybe b) -> [a] -> [b]
buildList _ [] = []
buildList f (x:xs) = case f x of Just y -> y:buildList f xs
                                 Nothing -> buildList f xs 

toBString :: String -> [Bit]
toBString = buildList toBit

toHString :: String -> [Hex]
toHString = buildList toHex

testLex :: String -> [ASN1Token] -> IO()
testLex input expected = assertEqual input expected (alexScanTokens input)

lexerTests = [testLex "TypeA := BOOLEAN"
                      [TypeOrModuleReferenceToken "TypeA", KeywordToken ":=", KeywordToken "BOOLEAN"],
              testLex "TypeA := TypeB"
                      [TypeOrModuleReferenceToken "TypeA", KeywordToken ":=", TypeOrModuleReferenceToken "TypeB"],
              testLex "TypeA := CHOICE { bool BOOLEAN }"
                      [TypeOrModuleReferenceToken "TypeA", KeywordToken ":=", KeywordToken "CHOICE", KeywordToken "{", IdentifierOrValueReferenceToken "bool", KeywordToken "BOOLEAN", KeywordToken "}"],
              testLex "INTEGER { two(2) }"
                      [KeywordToken "INTEGER", KeywordToken "{", IdentifierOrValueReferenceToken "two", KeywordToken "(", NumberToken 2, KeywordToken ")", KeywordToken "}"],
              testLex "SEQUENCE OF TypeB"
                      [KeywordToken "SEQUENCE", KeywordToken "OF", TypeOrModuleReferenceToken "TypeB"],
              testLex "SEQUENCE OF bool BOOLEAN"
                      [KeywordToken "SEQUENCE", KeywordToken "OF", IdentifierOrValueReferenceToken "bool", KeywordToken "BOOLEAN"],
              testLex "SEQUENCE { }"
                      [KeywordToken "SEQUENCE", KeywordToken "{", KeywordToken "}"],
              testLex "SEQUENCE { bool-A BOOLEAN , boolB BOOLEAN }"
                      [KeywordToken "SEQUENCE", KeywordToken "{", IdentifierOrValueReferenceToken "bool-A", KeywordToken "BOOLEAN", KeywordToken ",", IdentifierOrValueReferenceToken "boolB", KeywordToken "BOOLEAN", KeywordToken "}"],
              testLex "SEQUENCE { boolA BOOLEAN OPTIONAL }"
                      [KeywordToken "SEQUENCE", KeywordToken "{", IdentifierOrValueReferenceToken "boolA", KeywordToken "BOOLEAN", KeywordToken "OPTIONAL", KeywordToken "}"],
              testLex "test--test"
                      [IdentifierOrValueReferenceToken "test", KeywordToken "-", KeywordToken "-", IdentifierOrValueReferenceToken "test"],
              testLex "test..."
                      [IdentifierOrValueReferenceToken "test", KeywordToken "..."],
              testLex "test[[[["
                      [IdentifierOrValueReferenceToken "test", KeywordToken "[[", KeywordToken "[["],
              testLex "[[ 1: bool BOOLEAN ]]"
                      [KeywordToken "[[", NumberToken 1, KeywordToken ":", IdentifierOrValueReferenceToken "bool", KeywordToken "BOOLEAN", KeywordToken "]]"],
              testLex "\'\'B"
                      [BStringToken []],
              testLex "\'0 011\'B"
                      [BStringToken [B0, B0, B1, B1]],
              testLex "\'A3\'H"
                      [HStringToken [HA, H3]]
              ]
}