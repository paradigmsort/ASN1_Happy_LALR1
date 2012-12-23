{
module ASN1 where
import ASN1Lexer
import BasicTypes
import Data.List
import Test.HUnit
}

%name parse AssignmentList
%name parseBitStringValue BitStringValue
%name parseBooleanValue BooleanValue
%name parseChoiceValue ChoiceValue
%name parseIntegerValue IntegerValue
%name parseOctetStringValue OctetStringValue
%tokentype { ASN1Token }
%error { parseError }

%token
    TYPE_OR_MODULE_REFERENCE            { TypeOrModuleReferenceToken $$ }
    IDENTIFIER_OR_VALUE_REFERENCE       { IdentifierOrValueReferenceToken $$ }
    NUMBER                              { NumberToken $$ }
    BSTRING                             { BStringToken $$ }
    HSTRING                             { HStringToken $$ }
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
    'BIT'                               { KeywordToken "BIT" }
    'BOOLEAN'                           { KeywordToken "BOOLEAN" }
    'CHOICE'                            { KeywordToken "CHOICE" }
    'DEFAULT'                           { KeywordToken "DEFAULT"}
    'ENUMERATED'                        { KeywordToken "ENUMERATED" }
    'INTEGER'                           { KeywordToken "INTEGER" }
    'OCTET'                             { KeywordToken "OCTET" }
    'OF'                                { KeywordToken "OF" }
    'OPTIONAL'                          { KeywordToken "OPTIONAL" }
    'SEQUENCE'                          { KeywordToken "SEQUENCE" }
    'STRING'                            { KeywordToken "STRING" }
    'TRUE'                              { KeywordToken "TRUE" }
    'FALSE'                             { KeywordToken "FALSE" }
%%


AssignmentList : Assignment { [$1] }
               | AssignmentList Assignment { $1 ++ [$2] }

Assignment : TypeAssignment { WithRef $1 }
           | ValueAssignment { WithRef $1 }

TypeAssignment : TYPE_OR_MODULE_REFERENCE ':=' Type { TypeAssignment $1 $3 }

ValueAssignment : IDENTIFIER_OR_VALUE_REFERENCE Type ':=' Value { ValueAssignment $1 $2 $4 }

Type : BuiltinType { Builtin (TypeWithRef $1) }
     | ReferencedType { Reference $1 }

ReferencedType : DefinedType { $1 }

DefinedType : TYPE_OR_MODULE_REFERENCE { $1 }

NamedType : IDENTIFIER_OR_VALUE_REFERENCE Type { WithName $1 $2 }

BuiltinType : BitStringType { $1 }
            | BooleanType { $1 }
            | ChoiceType { $1 }
            | EnumeratedType { $1 }
            | IntegerType { $1 }
            | OctetStringType { $1 }
            | SequenceType { $1 }
            | SequenceOfType { $1 }

DefinedValue : IDENTIFIER_OR_VALUE_REFERENCE { $1 }

Value : BuiltinValue { $1 }

{-
Parses values of all types and retuns only the token sequence.
There are reduce/reduce conflicts between values that are resolved by knowing the type.
-}
BuiltinValue : BSTRING { [BStringToken $1] } --BitStringValue, OctetStringValue
             | HSTRING { [HStringToken $1] }
             | 'TRUE' { [KeywordToken "TRUE"] } --BooleanValue
             | 'FALSE' { [KeywordToken "FALSE"] }
             | IDENTIFIER_OR_VALUE_REFERENCE ':' Value { [IdentifierOrValueReferenceToken $1, KeywordToken ":"] ++ $3 } --ChoiceValue
             | NUMBER { [NumberToken $1] } --IntegerValue
             | '-' NUMBER { [KeywordToken "-", NumberToken $2] }
             | IDENTIFIER_OR_VALUE_REFERENCE { [IdentifierOrValueReferenceToken $1] }

BooleanType : 'BOOLEAN' { BooleanType }

BooleanValue : 'TRUE' { BooleanValue True }
             | 'FALSE' { BooleanValue False }

ChoiceType : 'CHOICE' '{' AlternativeTypeLists '}' { ChoiceType { choices = $3 } }

AlternativeTypeLists : RootAlternativeTypeList { $1 }

RootAlternativeTypeList : AlternativeTypeList { $1 }

AlternativeTypeList : NamedType { [$1] }
                    | AlternativeTypeList ',' NamedType { $1 ++ [$3] }

ChoiceValue : IDENTIFIER_OR_VALUE_REFERENCE ':' Value { ($1, $3) }

IntegerType : 'INTEGER' { IntegerType { namedIntegerValues = Nothing } }
            | 'INTEGER' '{' NamedNumberList '}' { IntegerType { namedIntegerValues = Just $3 } }

NamedNumberList : NamedNumber { [$1] }
                | NamedNumberList ',' NamedNumber { $1 ++ [$3] }

NamedNumber : IDENTIFIER_OR_VALUE_REFERENCE '(' SignedNumber ')' { WithName $1 (Builtin $3) }
            | IDENTIFIER_OR_VALUE_REFERENCE '(' DefinedValue ')' { WithName $1 (Reference $3) }

SignedNumber : NUMBER { $1 }
             | '-' NUMBER { (-$2) }

IntegerValue : SignedNumber { IntegerValue (Builtin $1) }
             | IDENTIFIER_OR_VALUE_REFERENCE { IntegerValue (Reference $1) }

EnumeratedType : 'ENUMERATED' '{' Enumerations '}' { EnumeratedType $3 } --TODO fix

Enumerations : RootEnumeration { $1 }

RootEnumeration : Enumeration { $1 }

Enumeration : EnumerationItem { [$1] }
            | EnumerationItem ',' Enumeration { $1:$3 }

EnumerationItem : IDENTIFIER_OR_VALUE_REFERENCE { UnnumberedEnumerationEntry $1 }
                | NamedNumber { NumberedEnumerationEntry $1 }

BitStringType : 'BIT' 'STRING' { BitStringType Nothing }
              | 'BIT' 'STRING' '{' NamedBitList '}' { BitStringType (Just $4) }

NamedBitList : NamedBit { [$1] }
             | NamedBitList ',' NamedBit { $1 ++ [$3] }

NamedBit : IDENTIFIER_OR_VALUE_REFERENCE '(' NUMBER ')' { WithName $1 (Builtin $3) }
         | IDENTIFIER_OR_VALUE_REFERENCE '(' DefinedValue ')' { WithName $1 (Reference $3) }

BitStringValue : BSTRING { BitStringValue $1 }
               | HSTRING { BitStringValue (hexesToBits $1) }

OctetStringType : 'OCTET' 'STRING' { OctetStringType }

OctetStringValue : BSTRING { OctetStringValue (bitsToOctets $1) }
                 | HSTRING { OctetStringValue ((bitsToOctets . hexesToBits) $1) }

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
              | NamedType 'DEFAULT' Value { Default $1 $3 }

SequenceOfType : 'SEQUENCE' 'OF' Type { SequenceOfType (Unnamed $3) }
               | 'SEQUENCE' 'OF' NamedType { SequenceOfType (Named $3) }

{
parseError :: [ASN1Token] -> a
parseError token = error ("Parse Error, remaining: " ++ show token)

data ASN1StagedAssignment a b = TypeAssignment { name :: String, asn1Type::a }
                              | ValueAssignment { name :: String, asn1Type::a, assignmentValue::b } deriving (Show, Eq)
data ASN1Assignment = WithRef (ASN1StagedAssignment (ASN1BuiltinOrReference ASN1TypeWithRef) [ASN1Token]) deriving (Show, Eq)
data ASN1TypeNoRefAssignment = NoRef (ASN1StagedAssignment ASN1TypeNoRef [ASN1Token]) deriving (Show, Eq)
data ASN1ValueParsedAssignment = ValParsed (ASN1StagedAssignment ASN1TypeValueParsed ASN1Value) deriving (Show, Eq)

data ASN1WithName a = WithName String a deriving (Show, Eq)
data ASN1OptionallyNamed a = Unnamed a
                           | Named (ASN1WithName a) deriving (Show, Eq)
data ASN1BuiltinOrReference a = Builtin a 
                              | Reference String deriving (Show, Eq)
data ASN1RequiredOptionalOrDefault b a = Required a
                                       | Optional a 
                                       | Default a b deriving (Show, Eq)
data ASN1EnumerationEntry = UnnumberedEnumerationEntry String
                          | NumberedEnumerationEntry (ASN1WithName (ASN1BuiltinOrReference Integer)) deriving (Show, Eq)
data ASN1StagedType a b = BitStringType { namedBits :: Maybe [ASN1WithName (ASN1BuiltinOrReference Integer)] }
                        | BooleanType
                        | ChoiceType { choices :: [ASN1WithName a] }
                        | EnumeratedType [ASN1EnumerationEntry]
                        | IntegerType { namedIntegerValues :: Maybe [ASN1WithName (ASN1BuiltinOrReference Integer)] }
                        | OctetStringType
                        | SequenceType {
                                         preExtensionComponents :: [ASN1RequiredOptionalOrDefault b (ASN1WithName a )],
                                         extensonAdditions :: [[ASN1RequiredOptionalOrDefault b (ASN1WithName a )]],
                                         postExtensionComponents :: [ASN1RequiredOptionalOrDefault b (ASN1WithName a )]
                                       }
                        | SequenceOfType (ASN1OptionallyNamed a) deriving (Show, Eq)

data ASN1TypeWithRef = TypeWithRef (ASN1StagedType (ASN1BuiltinOrReference ASN1TypeWithRef) [ASN1Token]) deriving (Show, Eq)
data ASN1TypeNoRef = TypeNoRef (ASN1StagedType ASN1TypeNoRef [ASN1Token]) deriving (Show, Eq)
data ASN1TypeValueParsed = TypeValParsed (ASN1StagedType ASN1TypeValueParsed ASN1Value) deriving (Show, Eq)

data ASN1Value = BitStringValue [Bit]
               | BooleanValue Bool
               | ChoiceValue { chosen :: String, choiceValue :: ASN1Value }
               | IntegerValue (ASN1BuiltinOrReference Integer)
               | OctetStringValue [Octet] deriving (Show, Eq)

instance Functor ASN1WithName where
  fmap f (WithName n x) = WithName n (f x)

instance Functor ASN1OptionallyNamed where
  fmap f (Unnamed x) = Unnamed (f x)
  fmap f (Named x) = Named (fmap f x)

instance Functor (ASN1RequiredOptionalOrDefault b) where
  fmap f (Required a) = Required (f a)
  fmap f (Optional a) = Optional (f a)
  fmap f (Default a b) = Default (f a) b

parseValueByType :: ASN1TypeNoRef -> [ASN1Token] -> ASN1Value
parseValueByType (TypeNoRef t) = case t of BitStringType _ -> parseBitStringValue
                                           BooleanType -> parseBooleanValue
                                           ChoiceType choices -> (\(choice, tokens) -> ChoiceValue choice (parseValueByType (findTypeInChoicesByName choices choice) tokens)) . parseChoiceValue 
                                           IntegerType _ -> parseIntegerValue
                                           OctetStringType -> parseOctetStringValue

parseValuesInROD :: ASN1RequiredOptionalOrDefault [ASN1Token] (ASN1WithName ASN1TypeNoRef) -> ASN1RequiredOptionalOrDefault ASN1Value (ASN1WithName ASN1TypeValueParsed)
parseValuesInROD (Required (WithName n t)) = Required (WithName n (parseValuesInType t))
parseValuesInROD (Optional (WithName n t)) = Optional (WithName n (parseValuesInType t))
parseValuesInROD (Default (WithName n t) v) = Default (WithName n (parseValuesInType t)) (parseValueByType t v)

parseValuesInType :: ASN1TypeNoRef -> ASN1TypeValueParsed
parseValuesInType (TypeNoRef t) = TypeValParsed (case t of BitStringType namedBits -> BitStringType namedBits
                                                           BooleanType -> BooleanType
                                                           ChoiceType choices -> ChoiceType (map (fmap parseValuesInType) choices)
                                                           EnumeratedType entries -> EnumeratedType entries
                                                           IntegerType namedIntegerValues -> IntegerType namedIntegerValues
                                                           OctetStringType -> OctetStringType
                                                           SequenceType pre ext post -> SequenceType (map parseValuesInROD pre)
                                                                                                     (map (map parseValuesInROD) ext)
                                                                                                     (map parseValuesInROD post)
                                                           SequenceOfType stype -> SequenceOfType (fmap parseValuesInType stype))

isNamed :: String -> ASN1WithName a -> Bool
isNamed name (WithName named _) = name == named

findTypeInChoicesByName :: [ASN1WithName ASN1TypeNoRef] -> String -> ASN1TypeNoRef
findTypeInChoicesByName as n = case find (isNamed n) as of Nothing -> error ("could not find choice " ++ n)
                                                           Just (WithName _ namedType) -> namedType

definesType :: String -> ASN1Assignment -> Bool
definesType name (WithRef (TypeAssignment n t)) = n == name
definesType name (WithRef (ValueAssignment _ _ _)) = False

findTypeByName :: [ASN1Assignment] -> String -> ASN1BuiltinOrReference ASN1TypeWithRef
findTypeByName as n = case find (definesType n) as of Nothing -> error ("could not resolve reference to type " ++ n)
                                                      Just (WithRef (TypeAssignment n t)) -> t

resolveTypeReference :: [ASN1Assignment] -> ASN1BuiltinOrReference ASN1TypeWithRef -> ASN1TypeWithRef
resolveTypeReference as (Builtin b) = b
resolveTypeReference as (Reference r) = ((resolveTypeReference as) . (findTypeByName as)) r

resolveTypeComponents :: [ASN1Assignment] -> ASN1TypeWithRef -> ASN1TypeNoRef
resolveTypeComponents as (TypeWithRef t) = TypeNoRef (case t of BitStringType namedBits -> BitStringType namedBits
                                                                BooleanType -> BooleanType
                                                                ChoiceType choices -> ChoiceType (map (fmap (resolveTypeCompletely as)) choices)
                                                                EnumeratedType entries -> EnumeratedType entries
                                                                IntegerType namedIntegerValues -> IntegerType namedIntegerValues
                                                                OctetStringType -> OctetStringType
                                                                SequenceType pre ext post -> SequenceType (map (fmap (fmap (resolveTypeCompletely as))) pre)
                                                                                                          (map (map (fmap (fmap (resolveTypeCompletely as)))) ext)
                                                                                                          (map (fmap (fmap (resolveTypeCompletely as))) post)
                                                                SequenceOfType stype -> SequenceOfType (fmap (resolveTypeCompletely as) stype))

resolveTypeCompletely :: [ASN1Assignment] -> ASN1BuiltinOrReference ASN1TypeWithRef -> ASN1TypeNoRef
resolveTypeCompletely as = (resolveTypeComponents as) . (resolveTypeReference as)

resolveTypesInAssignment :: [ASN1Assignment] -> ASN1Assignment -> ASN1TypeNoRefAssignment
resolveTypesInAssignment as = mapTypeInAssignment (resolveTypeCompletely as)

mapTypeInAssignment :: (ASN1BuiltinOrReference ASN1TypeWithRef -> ASN1TypeNoRef) -> ASN1Assignment -> ASN1TypeNoRefAssignment
mapTypeInAssignment f (WithRef (TypeAssignment n t)) = NoRef (TypeAssignment n (f t))
mapTypeInAssignment f (WithRef (ValueAssignment n t v)) = NoRef (ValueAssignment n (f t) v)

resolveTypes :: [ASN1Assignment] -> [ASN1TypeNoRefAssignment]
resolveTypes x = map (resolveTypesInAssignment x) x

parseValueInAssignment :: ASN1TypeNoRefAssignment -> ASN1ValueParsedAssignment
parseValueInAssignment (NoRef (TypeAssignment n t)) = ValParsed (TypeAssignment n (parseValuesInType t))
parseValueInAssignment (NoRef (ValueAssignment n t v)) = ValParsed (ValueAssignment n (parseValuesInType t) (parseValueByType t v))

parseValues :: [ASN1TypeNoRefAssignment] -> [ASN1ValueParsedAssignment]
parseValues x = map parseValueInAssignment x

testParse :: String -> [ASN1ValueParsedAssignment] -> IO ()
testParse input expected = assertEqual input expected ((parseValues . resolveTypes . parse . alexScanTokens) input)

tests = [testParse "TypeA := BOOLEAN"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed BooleanType))],
         testParse "TypeA := TypeB TypeB := BOOLEAN"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed BooleanType)), ValParsed (TypeAssignment "TypeB" (TypeValParsed BooleanType))],
         testParse "TypeA := CHOICE { bool BOOLEAN }"
                   [ValParsed (TypeAssignment "TypeA" ((TypeValParsed ChoiceType {choices=[WithName "bool" (TypeValParsed BooleanType)]})))],
         testParse "TypeA := INTEGER { two(2) }"
                   [ValParsed (TypeAssignment "TypeA" ((TypeValParsed IntegerType {namedIntegerValues=Just [WithName "two" (Builtin 2)]})))],
         testParse "TypeA := SEQUENCE OF TypeB TypeB := BOOLEAN"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceOfType (Unnamed (TypeValParsed BooleanType))))), ValParsed (TypeAssignment "TypeB" (TypeValParsed BooleanType))],
         testParse "TypeA := SEQUENCE OF bool BOOLEAN"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceOfType (Named (WithName "bool" (TypeValParsed BooleanType))))))],
         testParse "TypeA := SEQUENCE { }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [] [] [])))],
         testParse "TypeA := SEQUENCE { boolA BOOLEAN , boolB BOOLEAN }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [Required (WithName "boolA" (TypeValParsed BooleanType)),Required (WithName "boolB" (TypeValParsed BooleanType))] [] [])))],
         testParse "TypeA := SEQUENCE { boolA BOOLEAN OPTIONAL }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [Optional (WithName "boolA" (TypeValParsed BooleanType))] [] [])))],
         testParse "TypeA := SEQUENCE { boolA BOOLEAN, ... }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [Required (WithName "boolA" (TypeValParsed BooleanType))] [] [])))],
         testParse "TypeA := SEQUENCE { ... , ... , boolA BOOLEAN }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [] [] [Required (WithName "boolA" (TypeValParsed BooleanType))])))],
         testParse "TypeA := SEQUENCE { ... , boolA BOOLEAN , ... }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [] [[Required (WithName "boolA" (TypeValParsed BooleanType))]] [])))],
         testParse "TypeA := SEQUENCE OF CHOICE { b BOOLEAN , i INTEGER }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceOfType (Unnamed (TypeValParsed (ChoiceType {choices=[WithName "b" (TypeValParsed BooleanType), WithName "i" (TypeValParsed (IntegerType {namedIntegerValues=Nothing}))]}))))))],
         testParse "TypeA := SEQUENCE OF SEQUENCE { b BOOLEAN, ... , ...}"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceOfType (Unnamed (TypeValParsed (SequenceType [Required (WithName "b" (TypeValParsed BooleanType))] [] []))))))],
         testParse "TypeA := ENUMERATED { red, green }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (EnumeratedType [UnnumberedEnumerationEntry "red", UnnumberedEnumerationEntry "green"])))],
         testParse "TypeA := ENUMERATED { red(1), green, blue(2) }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (EnumeratedType [NumberedEnumerationEntry (WithName "red" (Builtin 1)), UnnumberedEnumerationEntry "green", NumberedEnumerationEntry (WithName "blue" (Builtin 2))])))],
         testParse "TypeA := BIT STRING"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (BitStringType {namedBits = Nothing})))],
         testParse "TypeA := BIT STRING { omit-start(0), omit-end(1) }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (BitStringType {namedBits = Just [WithName "omit-start" (Builtin 0), WithName "omit-end" (Builtin 1)]})))],
         testParse "TypeA := SEQUENCE OF OCTET STRING"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceOfType (Unnamed (TypeValParsed OctetStringType)))))],
         testParse "valueA BOOLEAN := TRUE"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed BooleanType, assignmentValue=BooleanValue True})],
         testParse "valueA TypeA := FALSE TypeA := BOOLEAN"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed BooleanType, assignmentValue=BooleanValue False}), (ValParsed TypeAssignment {name="TypeA", asn1Type=(TypeValParsed BooleanType)})],
         testParse "valueA INTEGER := -5"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed (IntegerType {namedIntegerValues=Nothing}), assignmentValue=IntegerValue (Builtin (-5))})],
         testParse "valueA INTEGER {five(5)} := two"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed (IntegerType {namedIntegerValues= Just [WithName "five" (Builtin 5)]}), assignmentValue=IntegerValue (Reference "two")})],
         testParse "valueA CHOICE {choiceA BOOLEAN } := choiceA : TRUE"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed (ChoiceType {choices=[WithName "choiceA" (TypeValParsed BooleanType)]}), assignmentValue=ChoiceValue "choiceA" (BooleanValue True)})],
         testParse "valueA BIT STRING := \'0000\'B"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed (BitStringType {namedBits=Nothing}), assignmentValue=BitStringValue [B0,B0,B0,B0]})],
         testParse "valueA BIT STRING := \'3\'H"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed (BitStringType {namedBits=Nothing}), assignmentValue=BitStringValue [B0,B0,B1,B1]})],
         testParse "TypeA := TypeC TypeB := OCTET STRING TypeC := TypeB"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed OctetStringType)), ValParsed (TypeAssignment "TypeB" (TypeValParsed OctetStringType)), ValParsed (TypeAssignment "TypeC" (TypeValParsed OctetStringType))],
         testParse "TypeA := CHOICE { c TypeB } TypeB := CHOICE { c TypeC } TypeC := BOOLEAN"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (ChoiceType {choices=[WithName "c" (TypeValParsed (ChoiceType {choices=[WithName "c" (TypeValParsed BooleanType)]}))]}))), ValParsed (TypeAssignment "TypeB" (TypeValParsed (ChoiceType {choices=[WithName "c" (TypeValParsed BooleanType)]}))), ValParsed (TypeAssignment "TypeC" (TypeValParsed BooleanType))],
         testParse "valueA OCTET STRING := \'1\'B"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed OctetStringType, assignmentValue=OctetStringValue[(B1,B0,B0,B0,B0,B0,B0,B0)]})],
         testParse "valueA OCTET STRING := \'33\'H"
                   [ValParsed (ValueAssignment {name="valueA", asn1Type=TypeValParsed OctetStringType, assignmentValue=OctetStringValue[(B0,B0,B1,B1,B0,B0,B1,B1)]})],
         testParse "TypeA := SEQUENCE { a BOOLEAN DEFAULT TRUE }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [Default (WithName "a" (TypeValParsed BooleanType)) (BooleanValue True)] [] [])))],
         testParse "TypeA := SEQUENCE { b BIT STRING DEFAULT \'10\'B }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceType [Default (WithName "b" (TypeValParsed BitStringType {namedBits = Nothing})) (BitStringValue [B1,B0])] [] [])))],
         testParse "TypeA := SEQUENCE OF CHOICE { s SEQUENCE { o OCTET STRING DEFAULT \'F\'H } }"
                   [ValParsed (TypeAssignment "TypeA" (TypeValParsed (SequenceOfType (Unnamed (TypeValParsed (ChoiceType {choices=[WithName "s" (TypeValParsed (SequenceType [Default (WithName "o" (TypeValParsed OctetStringType)) (OctetStringValue [(B1,B1,B1,B1,B0,B0,B0,B0)])] [] []))]}))))))]
        ] ++ lexerTests

main = foldr (>>) (putStrLn "OK") tests
}