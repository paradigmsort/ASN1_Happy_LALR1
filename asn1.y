{
module ASN1 where
import ASN1Lexer
import Test.HUnit
}

%name parse
%tokentype { ASN1Token }
%error { parseError }

%token
    TYPE_OR_MODULE_REFERENCE            { TypeOrModuleReferenceToken $$ }
    IDENTIFIER_OR_VALUE_REFERENCE       { IdentifierOrValueReferenceToken $$ }
    NUMBER                              { NumberToken $$ }
    ':='                                { KeywordToken ":=" }
    '...'                               { KeywordToken "..." }
    '[['                                { KeywordToken "[[" }
    ']]'                                { KeywordToken "]]" }
    '{'                                 { KeywordToken "{" }
    '}'                                 { KeywordToken "}" }
    ','                                 { KeywordToken "," }
    '('                                 { KeywordToken "(" }
    ')'                                 { KeywordToken ")" }
    '-'                                 { KeywordToken "-" }
    ':'                                 { KeywordToken ":" }
    'BOOLEAN'                           { KeywordToken "BOOLEAN" }
    'CHOICE'                            { KeywordToken "CHOICE" }
    'ENUMERATED'                        { KeywordToken "ENUMERATED" }
    'INTEGER'                           { KeywordToken "INTEGER" }
    'OF'                                { KeywordToken "OF" }
    'OPTIONAL'                          { KeywordToken "OPTIONAL" }
    'SEQUENCE'                          { KeywordToken "SEQUENCE" }
%%

TypeAssignment : TYPE_OR_MODULE_REFERENCE ':=' Type { TypeAssignment $1 $3 }

Type : BuiltinType { Value $1 }
     | ReferencedType { Reference $1 }

ReferencedType : DefinedType { $1 }

DefinedType : TYPE_OR_MODULE_REFERENCE { $1 }

NamedType : IDENTIFIER_OR_VALUE_REFERENCE Type { WithName $1 $2 }

BuiltinType : BooleanType { $1 }
            | ChoiceType { $1 }
            | EnumeratedType { $1 }
            | IntegerType { $1 }
            | SequenceType { $1 }
            | SequenceOfType { $1 }

DefinedValue : IDENTIFIER_OR_VALUE_REFERENCE { $1 }

BooleanType : 'BOOLEAN' { BooleanType }

ChoiceType : 'CHOICE' '{' AlternativeTypeLists '}' { ChoiceType $3 }

AlternativeTypeLists : RootAlternativeTypeList { $1 }

RootAlternativeTypeList : AlternativeTypeList { $1 }

AlternativeTypeList : NamedType { [$1] }
                    | AlternativeTypeList ',' NamedType { $1 ++ [$3] }

IntegerType : 'INTEGER' { IntegerType Nothing }
            | 'INTEGER' '{' NamedNumberList '}' { IntegerType (Just $3) }

NamedNumberList : NamedNumber { [$1] }
                | NamedNumberList ',' NamedNumber { $1 ++ [$3] }

NamedNumber : IDENTIFIER_OR_VALUE_REFERENCE '(' SignedNumber ')' { WithName $1 (Value $3) }
            | IDENTIFIER_OR_VALUE_REFERENCE '(' DefinedValue ')' { WithName $1 (Reference $3) }

SignedNumber : NUMBER { $1 }
             | '-' NUMBER { (-$2) }

EnumeratedType : 'ENUMERATED' '{' Enumerations '}' { EnumeratedType $3 } --TODO fix

Enumerations : RootEnumeration { $1 }

RootEnumeration : Enumeration { $1 }

Enumeration : EnumerationItem { [$1] }
            | EnumerationItem ',' Enumeration { $1:$3 }

EnumerationItem : IDENTIFIER_OR_VALUE_REFERENCE { UnnumberedEnumerationEntry $1 }
                | NamedNumber { NumberedEnumerationEntry $1 }

SequenceType : 'SEQUENCE' '{' '}' { SequenceType [] [] [] }
             | 'SEQUENCE' '{' ComponentTypeLists '}' { $3 }
          -- | 'SEQUENCE' '{' ExtensionAndException OptionalExtensionMarker '}' -- Covered below by empty ExtensionAdditions

ExtensionAndException : '...' {}

{- 
--Replaced directly with literal tokens for ExtensionEndMarker (',' '...') or nothing in ComponentTypeLists
OptionalExtensionMarker : {- Empty -} {}
                        | ExtensionEndMarker {}
-}

ComponentTypeLists : ComponentTypeList { SequenceType $1 [] [] }
                   | ComponentTypeList ',' ExtensionAndException { SequenceType $1 [] [] }
                   | ComponentTypeList ',' ExtensionAndException ',' '...' { SequenceType $1 [] [] }
                   | ComponentTypeList ',' ExtensionAndException ExtensionAdditions { SequenceType $1 $4 [] }
                   | ComponentTypeList ',' ExtensionAndException ExtensionAdditionsWithComma  '...' { SequenceType $1 $4 []}
                   | ComponentTypeList ',' ExtensionAndException ',' '...' ',' ComponentTypeList { SequenceType $1 [] $7 }
                   | ComponentTypeList ',' ExtensionAndException ExtensionAdditionsWithComma '...' ',' ComponentTypeList { SequenceType $1 $4 $7 }
                   | ExtensionAndException ',' '...' ',' ComponentTypeList { SequenceType [] [] $5 }
                   | ExtensionAndException ExtensionAdditionsWithComma '...' ',' ComponentTypeList { SequenceType [] $2 $5 }
                   | ExtensionAndException { SequenceType [] [] []}
                   | ExtensionAndException ',' '...' { SequenceType [] [] [] }
                   | ExtensionAndException ExtensionAdditions { SequenceType [] $2 [] }
                   | ExtensionAndException ExtensionAdditionsWithComma '...' { SequenceType [] $2 [] }

{-
--Irrelevant production that stops us from extending the list if reduced (shift/reduce conflict)
--Replaced directly with ComponentTypeList
RootComponentTypeList : ComponentTypeList { $1 }
-}

{-
--Replaced directly with literal tokens in ComponentTypeLists
ExtensionEndMarker : ',' '...' {}
-}

ExtensionAdditions : ',' ExtensionAdditionList { $2 }
{-                 | {- Empty -} { [] } -- Empty production removed and replaced with literal expansion in ComponentTypeLists-}

--New productions to give us +1 lookahead in this case by always shifting the comma
ExtensionAdditionsWithComma : ',' ExtensionAdditionListWithComma { $2 }
ExtensionAdditionListWithComma : ExtensionAdditionList ',' { $1 }

ExtensionAdditionList : ExtensionAddition { [$1] }
                      | ExtensionAdditionListWithComma ExtensionAddition { $1 ++ [$2] }

ExtensionAddition : ComponentType { [$1] }
                  | ExtensionAdditionGroup { $1 }

ExtensionAdditionGroup : '[[' VersionNumber ComponentTypeList ']]' { $3 }

VersionNumber : {- Empty -} {}
              | NUMBER ':' {}

ComponentTypeList : ComponentType { [$1] }
                  | ComponentTypeList ',' ComponentType { $1 ++ [$3] }

ComponentType : NamedType { Required $1 }
              | NamedType 'OPTIONAL' { Optional $1 }

SequenceOfType : 'SEQUENCE' 'OF' Type { SequenceOfType (Unnamed $3) }
               | 'SEQUENCE' 'OF' NamedType { SequenceOfType (Named $3) }

{
parseError :: [ASN1Token] -> a
parseError token = error ("Parse Error, remaining: " ++ show token)

data ASN1TypeAssignment = TypeAssignment String (ASN1ValueOrReference ASN1Type) deriving (Show, Eq)
data ASN1WithName a = WithName String a deriving (Show, Eq)
data ASN1OptionallyNamed a = Unnamed a
                           | Named (ASN1WithName a) deriving (Show, Eq)
data ASN1ValueOrReference a = Value a 
                            | Reference String deriving (Show, Eq)
data ASN1RequiredOrOptional a = Required a
                              | Optional a deriving (Show, Eq)
data ASN1EnumerationEntry = UnnumberedEnumerationEntry String
                          | NumberedEnumerationEntry (ASN1WithName (ASN1ValueOrReference Integer)) deriving (Show, Eq)
data ASN1Type = BooleanType
              | ChoiceType [ASN1WithName (ASN1ValueOrReference ASN1Type)]
              | EnumeratedType [ASN1EnumerationEntry]
              | IntegerType (Maybe [ASN1WithName (ASN1ValueOrReference Integer)])
              | SequenceType {
                               preExtensionComponents :: [ASN1RequiredOrOptional (ASN1WithName (ASN1ValueOrReference ASN1Type))],
                               extensonAdditions :: [[ASN1RequiredOrOptional (ASN1WithName (ASN1ValueOrReference ASN1Type))]],
                               postExtensionComponents :: [ASN1RequiredOrOptional (ASN1WithName (ASN1ValueOrReference ASN1Type))]
                             }
              | SequenceOfType (ASN1OptionallyNamed (ASN1ValueOrReference ASN1Type)) deriving (Show, Eq)

testParse :: String -> ASN1TypeAssignment  -> IO ()
testParse input expected = assertEqual input expected (parse (alexScanTokens input))

tests = [testParse "TypeA := BOOLEAN"
                   (TypeAssignment "TypeA" (Value BooleanType)),
         testParse "TypeA := TypeB"
                   (TypeAssignment "TypeA" (Reference "TypeB")),
         testParse "TypeA := CHOICE { bool BOOLEAN }"
                   (TypeAssignment "TypeA" (Value (ChoiceType [WithName "bool" (Value BooleanType)]))),
         testParse "TypeA := INTEGER { two(2) }"
                   (TypeAssignment "TypeA" (Value (IntegerType (Just [WithName "two" (Value 2)])))),
         testParse "TypeA := SEQUENCE OF TypeB"
                   (TypeAssignment "TypeA" (Value (SequenceOfType (Unnamed (Reference "TypeB"))))),
         testParse "TypeA := SEQUENCE OF bool BOOLEAN"
                   (TypeAssignment "TypeA" (Value (SequenceOfType (Named (WithName "bool" (Value BooleanType)))))),
         testParse "TypeA := SEQUENCE { }"
                   (TypeAssignment "TypeA" (Value (SequenceType [] [] []))),
         testParse "TypeA := SEQUENCE { boolA BOOLEAN , boolB BOOLEAN }"
                   (TypeAssignment "TypeA" (Value (SequenceType [Required (WithName "boolA" (Value BooleanType)),Required (WithName "boolB" (Value BooleanType))] [] []))),
         testParse "TypeA := SEQUENCE { boolA BOOLEAN OPTIONAL }"
                   (TypeAssignment "TypeA" (Value (SequenceType [Optional (WithName "boolA" (Value BooleanType))] [] []))),
         testParse "TypeA := SEQUENCE { boolA BOOLEAN, ... }"
                   (TypeAssignment "TypeA" (Value (SequenceType [Required (WithName "boolA" (Value BooleanType))] [] []))),
         testParse "TypeA := SEQUENCE { ... , ... , boolA BOOLEAN }"
                   (TypeAssignment "TypeA" (Value (SequenceType [] [] [Required (WithName "boolA" (Value BooleanType))]))),
         testParse "TypeA := SEQUENCE { ... , boolA BOOLEAN , ... }"
                   (TypeAssignment "TypeA" (Value (SequenceType [] [[Required (WithName "boolA" (Value BooleanType))]] []))),
         testParse "TypeA := SEQUENCE OF CHOICE { b BOOLEAN , i INTEGER }"
                   (TypeAssignment "TypeA" (Value (SequenceOfType (Unnamed (Value (ChoiceType [WithName "b" (Value BooleanType), WithName "i" (Value (IntegerType Nothing))])))))),
         testParse "TypeA := SEQUENCE OF SEQUENCE { b BOOLEAN, ... , ...}"
                   (TypeAssignment "TypeA" (Value (SequenceOfType (Unnamed (Value (SequenceType [Required (WithName "b" (Value BooleanType))] [] [])))))),
         testParse "TypeA := ENUMERATED { red, green }"
                   (TypeAssignment "TypeA" (Value (EnumeratedType [UnnumberedEnumerationEntry "red", UnnumberedEnumerationEntry "green"])))
        ] ++ lexerTests

main = foldr (>>) (putStrLn "OK") tests
}