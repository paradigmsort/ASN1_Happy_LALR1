{
module ASN1Lexer where
import Test.HUnit
}

%wrapper "basic"

$digit = 0-9
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
    \'( 0 | 1 | $white )*\'B        { BStringToken . toBString }

{
data Bit = Zero | One deriving (Show, Eq)
data ASN1Token = TypeOrModuleReferenceToken String
               | IdentifierOrValueReferenceToken String
               | NumberToken Integer
               | BStringToken [Bit]
               | KeywordToken String deriving (Show, Eq)

toBString :: String -> [Bit]
toBString [] = []
toBString ('\'':xs) = toBString xs
toBString ('B':xs) = toBString xs
toBString ('0':xs) = Zero : toBString xs
toBString ('1':xs) = One : toBString xs
toBString (x:xs) = toBString xs

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
                      [BStringToken [Zero, Zero, One, One]]
              ]

}