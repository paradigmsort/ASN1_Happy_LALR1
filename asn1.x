{
module ASN1Lexer where
import qualified Data.Map
import Test.HUnit
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
data Bit = B0 | B1 deriving (Show, Eq)
data Hex = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | HA | HB | HC | HD | HE | HF deriving (Show, Eq)
data ASN1Token = TypeOrModuleReferenceToken String
               | IdentifierOrValueReferenceToken String
               | NumberToken Integer
               | BStringToken [Bit]
               | HStringToken [Hex]
               | KeywordToken String deriving (Show, Eq)

toBit :: Char -> Maybe Bit
toBit = (flip Data.Map.lookup) (Data.Map.fromList [('0', B0), ('1', B1)])

toHex :: Char -> Maybe Hex
toHex = (flip Data.Map.lookup) (Data.Map.fromList [('0', H0), ('1', H1), ('2', H2), ('3', H3),
                                                   ('4', H4), ('5', H5), ('6', H6), ('7', H7),
                                                   ('8', H8), ('9', H9), ('A', HA), ('B', HB),
                                                   ('C', HC), ('D', HD), ('E', HE), ('F', HF)])

hexesToBits :: [Hex] -> [Bit]
hexesToBits = concat . map (\x -> case x of H0 -> [B0,B0,B0,B0]; H1 -> [B0,B0,B0,B1]; H2 -> [B0,B0,B1,B0]; H3 -> [B0,B0,B1,B1]
                                            H4 -> [B0,B1,B0,B0]; H5 -> [B0,B1,B0,B1]; H6 -> [B0,B1,B1,B0]; H7 -> [B0,B1,B1,B1]
                                            H8 -> [B1,B0,B0,B0]; H9 -> [B1,B0,B0,B1]; HA -> [B1,B0,B1,B0]; HB -> [B1,B0,B1,B1]
                                            HC -> [B1,B1,B0,B0]; HD -> [B1,B1,B0,B1]; HE -> [B1,B1,B1,B0]; HF -> [B1,B1,B1,B1])

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