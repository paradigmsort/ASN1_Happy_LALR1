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
%name parseEnumeratedValue EnumeratedValue
%name parseIntegerValue IntegerValue
%name parseNullValue NullValue
%name parseOctetStringValue OctetStringValue
%name parseSequenceValue SequenceValue
%name parseSequenceOfValue SequenceOfValue
%tokentype { ASN1Token }
%error { parseError }

%token
    TYPE_OR_MODULE_REFERENCE            { TypeOrModuleReferenceToken $$ }
    IDENTIFIER_OR_VALUE_REFERENCE       { IdentifierOrValueReferenceToken $$ }
    NUMBER                              { NumberToken $$ }
    BSTRING                             { BStringToken $$ }
    HSTRING                             { HStringToken $$ }
    '::='                               { KeywordToken "::=" }
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
    'NULL'                              { KeywordToken "NULL" }
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

TypeAssignment : TYPE_OR_MODULE_REFERENCE '::=' Type { TypeAssignment $1 $3 }

ValueAssignment : IDENTIFIER_OR_VALUE_REFERENCE Type '::=' Value { ValueAssignment $1 $2 $4 }

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
            | NullType { $1 }
            | OctetStringType { $1 }
            | SequenceType { $1 }
            | SequenceOfType { $1 }

Value : BuiltinValue { $1 }

DefinedValue : IDENTIFIER_OR_VALUE_REFERENCE { $1 }

NamedValue : IDENTIFIER_OR_VALUE_REFERENCE Value { ($1, $2) }
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
             | IDENTIFIER_OR_VALUE_REFERENCE { [IdentifierOrValueReferenceToken $1] } -- IntegerValue, EnumeratedValue
             | '{' '}' { [KeywordToken "{", KeywordToken "}"] } --SequenceOfValue
             | '{' BuiltinValueList '}' { [KeywordToken "{"] ++ $2 ++ [KeywordToken "}"] }
             | '{' NamedBuiltinValueList '}' { [KeywordToken "{"] ++ $2 ++ [KeywordToken "}"] }
             | 'NULL' { [KeywordToken "NULL"] } --NullValue

BuiltinValueList : BuiltinValue { $1 }
                 | BuiltinValueList ',' BuiltinValue { $1 ++ [KeywordToken ","] ++ $3 }

NamedBuiltinValueList : NamedBuiltinValue { $1 }
                      | NamedBuiltinValueList ',' NamedBuiltinValue { $1 ++ [KeywordToken ","] ++ $3 }

NamedBuiltinValue : IDENTIFIER_OR_VALUE_REFERENCE BuiltinValue { [IdentifierOrValueReferenceToken $1] ++ $2 }

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

EnumeratedType : 'ENUMERATED' '{' Enumerations '}' { $3 }

Enumerations : Enumeration { EnumeratedType $1 Nothing }
             | Enumeration ',' '...' { EnumeratedType $1 (Just [])}
             | Enumeration ',' '...' ',' Enumeration { EnumeratedType $1 (Just $5)}

Enumeration : EnumerationItem { [$1] }
            | Enumeration ',' EnumerationItem { $1 ++ [$3] }

EnumerationItem : IDENTIFIER_OR_VALUE_REFERENCE { UnnumberedEnumerationEntry $1 }
                | NamedNumber { NumberedEnumerationEntry $1 }

EnumeratedValue : IDENTIFIER_OR_VALUE_REFERENCE { EnumeratedValue $1 }

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

NullType : 'NULL' { NullType }

NullValue : 'NULL' { NullValue }

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

SequenceValue : '{' '}' { [] }
              | '{' ComponentValueList '}' { $2 }

ComponentValueList : NamedValue { [$1] }
                   | ComponentValueList ',' NamedValue { $1 ++ [$3] }

SequenceOfType : 'SEQUENCE' 'OF' Type { SequenceOfType (Unnamed $3) }
               | 'SEQUENCE' 'OF' NamedType { SequenceOfType (Named $3) }

SequenceOfValue : '{' '}' { [] }
                | '{' ValueList '}' { $2 }

ValueList : Value { [$1] }
          | ValueList ',' Value { $1 ++ [$3] }

{
parseError :: [ASN1Token] -> a
parseError token = error ("Parse Error, remaining: " ++ show token)

data ASN1WithName a = WithName String a deriving (Show, Eq)
data ASN1OptionallyNamed a = Unnamed a
                           | Named (ASN1WithName a) deriving (Show, Eq)
data ASN1BuiltinOrReference a = Builtin a 
                              | Reference String deriving (Show, Eq)
data ASN1RequiredOptionalOrDefault b a = Required a
                                       | Optional a 
                                       | Default a b deriving (Show, Eq)

data ASN1StagedAssignment a b = TypeAssignment { name :: String, asn1Type::a }
                              | ValueAssignment { name :: String, asn1Type::a, assignmentValue::b } deriving (Show, Eq)
data ASN1Assignment = WithRef (ASN1StagedAssignment (ASN1BuiltinOrReference ASN1TypeWithRef) [ASN1Token]) deriving (Show, Eq)
data ASN1TypeNoRefAssignment = NoRef (ASN1StagedAssignment ASN1TypeNoRef [ASN1Token]) deriving (Show, Eq)
data ASN1ValueParsedAssignment = ValParsed (ASN1StagedAssignment ASN1TypeValueParsed ASN1ParsedValue) deriving (Show, Eq)
data ASN1DoneAssignment = Assignment (ASN1StagedAssignment ASN1Type ASN1Value) deriving (Show, Eq)

data ASN1EnumerationEntry = UnnumberedEnumerationEntry String
                          | NumberedEnumerationEntry (ASN1WithName (ASN1BuiltinOrReference Integer)) deriving (Show, Eq)

data ASN1StagedType a b c = BitStringType { namedBits :: Maybe [ASN1WithName c] }
                          | BooleanType
                          | ChoiceType { choices :: [ASN1WithName a] }
                          | EnumeratedType { rootEnumeration :: [ASN1EnumerationEntry], extensionEnumeration :: Maybe [ASN1EnumerationEntry] }
                          | IntegerType { namedIntegerValues :: Maybe [ASN1WithName c] }
                          | NullType
                          | OctetStringType
                          | SequenceType {
                                           preExtensionComponents :: [ASN1RequiredOptionalOrDefault b (ASN1WithName a )],
                                           extensonAdditions :: [[ASN1RequiredOptionalOrDefault b (ASN1WithName a )]],
                                           postExtensionComponents :: [ASN1RequiredOptionalOrDefault b (ASN1WithName a )]
                                         }
                          | SequenceOfType (ASN1OptionallyNamed a) deriving (Show, Eq)
data ASN1TypeWithRef = TypeWithRef (ASN1StagedType (ASN1BuiltinOrReference ASN1TypeWithRef) [ASN1Token] (ASN1BuiltinOrReference Integer)) deriving (Show, Eq)
data ASN1TypeNoRef = TypeNoRef (ASN1StagedType ASN1TypeNoRef [ASN1Token] (ASN1BuiltinOrReference Integer)) deriving (Show, Eq)
data ASN1TypeValueParsed = TypeValParsed (ASN1StagedType ASN1TypeValueParsed ASN1ParsedValue (ASN1BuiltinOrReference Integer)) deriving (Show, Eq)
data ASN1Type = Type (ASN1StagedType ASN1Type ASN1Value Integer) deriving (Show, Eq)

data ASN1StagedValue a = BitStringValue [Bit]
                       | BooleanValue Bool
                       | ChoiceValue { chosen :: String, choiceValue :: ASN1StagedValue a }
                       | EnumeratedValue String
                       | IntegerValue a
                       | NullValue
                       | OctetStringValue [Octet]
                       | SequenceValue [ASN1WithName (ASN1StagedValue a)]
                       | SequenceOfValue [ASN1StagedValue a] deriving (Show, Eq)
type ASN1ParsedValue = ASN1StagedValue (ASN1BuiltinOrReference Integer)
type ASN1Value = ASN1StagedValue Integer

instance Functor ASN1WithName where
  fmap f (WithName n x) = WithName n (f x)

instance Functor ASN1OptionallyNamed where
  fmap f (Unnamed x) = Unnamed (f x)
  fmap f (Named x) = Named (fmap f x)

instance Functor (ASN1RequiredOptionalOrDefault b) where
  fmap f (Required a) = Required (f a)
  fmap f (Optional a) = Optional (f a)
  fmap f (Default a b) = Default (f a) b

fromOptionallyNamed :: ASN1OptionallyNamed a -> a
fromOptionallyNamed (Unnamed a) = a
fromOptionallyNamed (Named (WithName n a)) = a

resolveIntLocal :: Maybe [ASN1WithName (ASN1BuiltinOrReference Integer)] -> ASN1ParsedValue -> ASN1ParsedValue
resolveIntLocal (Just xs) (IntegerValue (Reference n)) = case find (isNamed n) xs of Nothing -> IntegerValue (Reference n)
                                                                                     Just (WithName _ (Builtin v)) -> IntegerValue (Builtin v)
                                                                                     Just (WithName _ (Reference r)) -> resolveIntLocal (Just xs) (IntegerValue (Reference r))
resolveIntLocal _ x = x

parseValueByType :: ASN1TypeNoRef -> [ASN1Token] -> ASN1ParsedValue
parseValueByType (TypeNoRef t) = case t of BitStringType _ -> parseBitStringValue
                                           BooleanType -> parseBooleanValue
                                           ChoiceType choices -> (\(choice, tokens) -> ChoiceValue choice (parseValueByType (findByName choices choice) tokens)) . parseChoiceValue 
                                           EnumeratedType _ _ -> parseEnumeratedValue
                                           IntegerType mlist -> (resolveIntLocal mlist) . parseIntegerValue
                                           NullType -> parseNullValue
                                           OctetStringType -> parseOctetStringValue
                                           SequenceType pre ext post -> SequenceValue . (map (\(name, tokens) -> WithName name (parseValueByType (findTypeInSequenceByName (pre ++ concat ext ++ post) name) tokens))) . parseSequenceValue
                                           SequenceOfType stype -> SequenceOfValue . (map (parseValueByType (fromOptionallyNamed stype))) . parseSequenceOfValue

parseValuesInROD :: ASN1RequiredOptionalOrDefault [ASN1Token] (ASN1WithName ASN1TypeNoRef) -> ASN1RequiredOptionalOrDefault ASN1ParsedValue (ASN1WithName ASN1TypeValueParsed)
parseValuesInROD (Required (WithName n t)) = Required (WithName n (parseValuesInType t))
parseValuesInROD (Optional (WithName n t)) = Optional (WithName n (parseValuesInType t))
parseValuesInROD (Default (WithName n t) v) = Default (WithName n (parseValuesInType t)) (parseValueByType t v)

parseValuesInType :: ASN1TypeNoRef -> ASN1TypeValueParsed
parseValuesInType (TypeNoRef t) = TypeValParsed (case t of BitStringType namedBits -> BitStringType namedBits
                                                           BooleanType -> BooleanType
                                                           ChoiceType choices -> ChoiceType (map (fmap parseValuesInType) choices)
                                                           EnumeratedType entries ext -> EnumeratedType entries ext
                                                           IntegerType namedIntegerValues -> IntegerType namedIntegerValues
                                                           NullType -> NullType
                                                           OctetStringType -> OctetStringType
                                                           SequenceType pre ext post -> SequenceType (map parseValuesInROD pre)
                                                                                                     (map (map parseValuesInROD) ext)
                                                                                                     (map parseValuesInROD post)
                                                           SequenceOfType stype -> SequenceOfType (fmap parseValuesInType stype))

resolveValuesInROD :: [ASN1ValueParsedAssignment] -> ASN1RequiredOptionalOrDefault ASN1ParsedValue (ASN1WithName ASN1TypeValueParsed) -> ASN1RequiredOptionalOrDefault ASN1Value (ASN1WithName ASN1Type)
resolveValuesInROD as (Required (WithName n t)) = Required (WithName n (resolveValuesInType as t))
resolveValuesInROD as (Optional (WithName n t)) = Optional (WithName n (resolveValuesInType as t))
resolveValuesInROD as (Default (WithName n t) v) = Default (WithName n (resolveValuesInType as t)) (resolveValueComponents as v)

resolveValuesInType :: [ASN1ValueParsedAssignment] -> ASN1TypeValueParsed -> ASN1Type
resolveValuesInType as (TypeValParsed t) = Type (case t of BitStringType namedBits -> BitStringType (fmap (map (fmap (resolveIntegerReference as))) namedBits)
                                                           BooleanType -> BooleanType
                                                           ChoiceType choices -> ChoiceType (map (fmap (resolveValuesInType as)) choices)
                                                           EnumeratedType entries ext -> EnumeratedType entries ext
                                                           IntegerType namedIntegerValues -> IntegerType (fmap (map (fmap (resolveIntegerReference as))) namedIntegerValues)
                                                           NullType -> NullType
                                                           OctetStringType -> OctetStringType
                                                           SequenceType pre ext post -> SequenceType (map (resolveValuesInROD as) pre)
                                                                                                     (map (map (resolveValuesInROD as)) ext)
                                                                                                     (map (resolveValuesInROD as) post)
                                                           SequenceOfType stype -> SequenceOfType (fmap (resolveValuesInType as) stype))

stripROD :: ASN1RequiredOptionalOrDefault b a -> a
stripROD (Required a) =  a
stripROD (Optional a) =  a
stripROD (Default a b) = a

isNamed :: String -> ASN1WithName a -> Bool
isNamed name (WithName named _) = name == named

findByName :: [ASN1WithName a] -> String -> a
findByName as n = case find (isNamed n) as of Nothing -> error ("could not find" ++ n)
                                              Just (WithName _ a) -> a

findTypeInSequenceByName :: [ASN1RequiredOptionalOrDefault v (ASN1WithName ASN1TypeNoRef)] -> String -> ASN1TypeNoRef
findTypeInSequenceByName as = findByName (map stripROD as)

definesType :: String -> ASN1Assignment -> Bool
definesType name (WithRef (TypeAssignment n t)) = n == name
definesType name (WithRef (ValueAssignment _ _ _)) = False

findTypeByName :: [ASN1Assignment] -> String -> ASN1BuiltinOrReference ASN1TypeWithRef
findTypeByName as n = case find (definesType n) as of Nothing -> error ("could not resolve reference to type " ++ n)
                                                      Just (WithRef (TypeAssignment n t)) -> t

resolveTypeReference :: [ASN1Assignment] -> ASN1BuiltinOrReference ASN1TypeWithRef -> ASN1TypeWithRef
resolveTypeReference as (Builtin b) = b
resolveTypeReference as (Reference r) = ((resolveTypeReference as) . (findTypeByName as)) r

isIntegerNamed :: String -> ASN1ValueParsedAssignment -> Bool
isIntegerNamed name (ValParsed (ValueAssignment n _ (IntegerValue _))) = n == name
isIntegerNamed name _ = False

findIntegerByName :: [ASN1ValueParsedAssignment] -> String -> ASN1BuiltinOrReference Integer
findIntegerByName as n = case find (isIntegerNamed n) as of Nothing -> error ("could not resolve reference to integer " ++ n)
                                                            Just (ValParsed (ValueAssignment _ _ (IntegerValue r))) -> r

resolveIntegerReference :: [ASN1ValueParsedAssignment] -> ASN1BuiltinOrReference Integer -> Integer
resolveIntegerReference _ (Builtin i) = i
resolveIntegerReference as (Reference r) = ((resolveIntegerReference as) . (findIntegerByName as)) r

resolveValueComponents :: [ASN1ValueParsedAssignment] -> ASN1ParsedValue -> ASN1Value
resolveValueComponents as t = case t of BitStringValue bits -> BitStringValue bits
                                        BooleanValue b -> BooleanValue b
                                        ChoiceValue chosen choice -> ChoiceValue chosen (resolveValueComponents as choice)
                                        EnumeratedValue name -> EnumeratedValue name
                                        IntegerValue refval -> IntegerValue (resolveIntegerReference as refval)
                                        NullValue -> NullValue
                                        OctetStringValue octets -> OctetStringValue octets
                                        SequenceValue namedValues -> SequenceValue (map (fmap (resolveValueComponents as)) namedValues)
                                        SequenceOfValue values -> SequenceOfValue (map (resolveValueComponents as) values)

resolveTypeComponents :: [ASN1Assignment] -> ASN1TypeWithRef -> ASN1TypeNoRef
resolveTypeComponents as (TypeWithRef t) = TypeNoRef (case t of BitStringType namedBits -> BitStringType namedBits
                                                                BooleanType -> BooleanType
                                                                ChoiceType choices -> ChoiceType (map (fmap (resolveTypeCompletely as)) choices)
                                                                EnumeratedType entries ext -> EnumeratedType entries ext
                                                                IntegerType namedIntegerValues -> IntegerType namedIntegerValues
                                                                NullType -> NullType
                                                                OctetStringType -> OctetStringType
                                                                SequenceType pre ext post -> SequenceType (map (fmap (fmap (resolveTypeCompletely as))) pre)
                                                                                                          (map (map (fmap (fmap (resolveTypeCompletely as)))) ext)
                                                                                                          (map (fmap (fmap (resolveTypeCompletely as))) post)
                                                                SequenceOfType stype -> SequenceOfType (fmap (resolveTypeCompletely as) stype))

resolveTypeCompletely :: [ASN1Assignment] -> ASN1BuiltinOrReference ASN1TypeWithRef -> ASN1TypeNoRef
resolveTypeCompletely as = (resolveTypeComponents as) . (resolveTypeReference as)

mapTypeInAssignment :: (ASN1BuiltinOrReference ASN1TypeWithRef -> ASN1TypeNoRef) -> ASN1Assignment -> ASN1TypeNoRefAssignment
mapTypeInAssignment f (WithRef (TypeAssignment n t)) = NoRef (TypeAssignment n (f t))
mapTypeInAssignment f (WithRef (ValueAssignment n t v)) = NoRef (ValueAssignment n (f t) v)

resolveTypesInAssignment :: [ASN1Assignment] -> ASN1Assignment -> ASN1TypeNoRefAssignment
resolveTypesInAssignment as = mapTypeInAssignment (resolveTypeCompletely as)

resolveTypes :: [ASN1Assignment] -> [ASN1TypeNoRefAssignment]
resolveTypes x = map (resolveTypesInAssignment x) x

parseValueInAssignment :: ASN1TypeNoRefAssignment -> ASN1ValueParsedAssignment
parseValueInAssignment (NoRef (TypeAssignment n t)) = ValParsed (TypeAssignment n (parseValuesInType t))
parseValueInAssignment (NoRef (ValueAssignment n t v)) = ValParsed (ValueAssignment n (parseValuesInType t) (parseValueByType t v))

parseValues :: [ASN1TypeNoRefAssignment] -> [ASN1ValueParsedAssignment]
parseValues = map parseValueInAssignment

resolveValuesInAssignment :: [ASN1ValueParsedAssignment] -> ASN1ValueParsedAssignment -> ASN1DoneAssignment
resolveValuesInAssignment as (ValParsed (TypeAssignment n t)) = Assignment (TypeAssignment n (resolveValuesInType as t))
resolveValuesInAssignment as (ValParsed (ValueAssignment n t v)) = Assignment (ValueAssignment n (resolveValuesInType as t) (resolveValueComponents as v))

resolveValues :: [ASN1ValueParsedAssignment] -> [ASN1DoneAssignment]
resolveValues x = map (resolveValuesInAssignment x) x

testParse :: String -> [ASN1DoneAssignment] -> IO ()
testParse input expected = assertEqual input expected ((resolveValues . parseValues . resolveTypes . parse . alexScanTokens) input)

tests = [testParse "TypeA ::= BOOLEAN"
                   [Assignment (TypeAssignment "TypeA" (Type BooleanType))],
         testParse "TypeA ::= TypeB TypeB ::= BOOLEAN"
                   [Assignment (TypeAssignment "TypeA" (Type BooleanType)), Assignment (TypeAssignment "TypeB" (Type BooleanType))],
         testParse "TypeA ::= CHOICE { bool BOOLEAN }"
                   [Assignment (TypeAssignment "TypeA" ((Type ChoiceType {choices=[WithName "bool" (Type BooleanType)]})))],
         testParse "TypeA ::= INTEGER { two(2) }"
                   [Assignment (TypeAssignment "TypeA" ((Type IntegerType {namedIntegerValues=Just [WithName "two" 2]})))],
         testParse "TypeA ::= SEQUENCE OF TypeB TypeB ::= BOOLEAN"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceOfType (Unnamed (Type BooleanType))))), Assignment (TypeAssignment "TypeB" (Type BooleanType))],
         testParse "TypeA ::= SEQUENCE OF bool BOOLEAN"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceOfType (Named (WithName "bool" (Type BooleanType))))))],
         testParse "TypeA ::= SEQUENCE { }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [] [] [])))],
         testParse "TypeA ::= SEQUENCE { boolA BOOLEAN , boolB BOOLEAN }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [Required (WithName "boolA" (Type BooleanType)),Required (WithName "boolB" (Type BooleanType))] [] [])))],
         testParse "TypeA ::= SEQUENCE { boolA BOOLEAN OPTIONAL }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [Optional (WithName "boolA" (Type BooleanType))] [] [])))],
         testParse "TypeA ::= SEQUENCE { boolA BOOLEAN, ... }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [Required (WithName "boolA" (Type BooleanType))] [] [])))],
         testParse "TypeA ::= SEQUENCE { ... , ... , boolA BOOLEAN }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [] [] [Required (WithName "boolA" (Type BooleanType))])))],
         testParse "TypeA ::= SEQUENCE { ... , boolA BOOLEAN , ... }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [] [[Required (WithName "boolA" (Type BooleanType))]] [])))],
         testParse "TypeA ::= SEQUENCE OF CHOICE { b BOOLEAN , i INTEGER }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceOfType (Unnamed (Type (ChoiceType {choices=[WithName "b" (Type BooleanType), WithName "i" (Type (IntegerType {namedIntegerValues=Nothing}))]}))))))],
         testParse "TypeA ::= SEQUENCE OF SEQUENCE { b BOOLEAN, ... , ...}"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceOfType (Unnamed (Type (SequenceType [Required (WithName "b" (Type BooleanType))] [] []))))))],
         testParse "TypeA ::= ENUMERATED { red, green }"
                   [Assignment (TypeAssignment "TypeA" (Type (EnumeratedType [UnnumberedEnumerationEntry "red", UnnumberedEnumerationEntry "green"] Nothing)))],
         testParse "TypeA ::= ENUMERATED { red(1), green, blue(2) }"
                   [Assignment (TypeAssignment "TypeA" (Type (EnumeratedType [NumberedEnumerationEntry (WithName "red" (Builtin 1)), UnnumberedEnumerationEntry "green", NumberedEnumerationEntry (WithName "blue" (Builtin 2))] Nothing)))],
         testParse "TypeA ::= BIT STRING"
                   [Assignment (TypeAssignment "TypeA" (Type (BitStringType {namedBits = Nothing})))],
         testParse "TypeA ::= BIT STRING { omit-start(0), omit-end(1) }"
                   [Assignment (TypeAssignment "TypeA" (Type (BitStringType {namedBits = Just [WithName "omit-start" 0, WithName "omit-end" 1]})))],
         testParse "TypeA ::= SEQUENCE OF OCTET STRING"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceOfType (Unnamed (Type OctetStringType)))))],
         testParse "valueA BOOLEAN ::= TRUE"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type BooleanType, assignmentValue=BooleanValue True})],
         testParse "valueA TypeA ::= FALSE TypeA ::= BOOLEAN"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type BooleanType, assignmentValue=BooleanValue False}), (Assignment TypeAssignment {name="TypeA", asn1Type=(Type BooleanType)})],
         testParse "valueA INTEGER ::= -5"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (IntegerType {namedIntegerValues=Nothing}), assignmentValue=IntegerValue (-5)})],
         testParse "valueA INTEGER {five(5)} ::= two two INTEGER ::= 2"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (IntegerType {namedIntegerValues= Just [WithName "five" 5]}), assignmentValue=IntegerValue 2}), Assignment (ValueAssignment {name="two", asn1Type=Type (IntegerType Nothing), assignmentValue= IntegerValue 2})],
         testParse "valueA CHOICE {choiceA BOOLEAN } ::= choiceA : TRUE"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (ChoiceType {choices=[WithName "choiceA" (Type BooleanType)]}), assignmentValue=ChoiceValue "choiceA" (BooleanValue True)})],
         testParse "valueA BIT STRING ::= \'0000\'B"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (BitStringType {namedBits=Nothing}), assignmentValue=BitStringValue [B0,B0,B0,B0]})],
         testParse "valueA BIT STRING ::= \'3\'H"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (BitStringType {namedBits=Nothing}), assignmentValue=BitStringValue [B0,B0,B1,B1]})],
         testParse "TypeA ::= TypeC TypeB ::= OCTET STRING TypeC ::= TypeB"
                   [Assignment (TypeAssignment "TypeA" (Type OctetStringType)), Assignment (TypeAssignment "TypeB" (Type OctetStringType)), Assignment (TypeAssignment "TypeC" (Type OctetStringType))],
         testParse "TypeA ::= CHOICE { c TypeB } TypeB ::= CHOICE { c TypeC } TypeC ::= BOOLEAN"
                   [Assignment (TypeAssignment "TypeA" (Type (ChoiceType {choices=[WithName "c" (Type (ChoiceType {choices=[WithName "c" (Type BooleanType)]}))]}))), Assignment (TypeAssignment "TypeB" (Type (ChoiceType {choices=[WithName "c" (Type BooleanType)]}))), Assignment (TypeAssignment "TypeC" (Type BooleanType))],
         testParse "valueA OCTET STRING ::= \'1\'B"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type OctetStringType, assignmentValue=OctetStringValue[(B1,B0,B0,B0,B0,B0,B0,B0)]})],
         testParse "valueA OCTET STRING ::= \'33\'H"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type OctetStringType, assignmentValue=OctetStringValue[(B0,B0,B1,B1,B0,B0,B1,B1)]})],
         testParse "TypeA ::= SEQUENCE { a BOOLEAN DEFAULT TRUE }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [Default (WithName "a" (Type BooleanType)) (BooleanValue True)] [] [])))],
         testParse "TypeA ::= SEQUENCE { b BIT STRING DEFAULT \'10\'B }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceType [Default (WithName "b" (Type BitStringType {namedBits = Nothing})) (BitStringValue [B1,B0])] [] [])))],
         testParse "TypeA ::= SEQUENCE OF CHOICE { s SEQUENCE { o OCTET STRING DEFAULT \'F\'H } }"
                   [Assignment (TypeAssignment "TypeA" (Type (SequenceOfType (Unnamed (Type (ChoiceType {choices=[WithName "s" (Type (SequenceType [Default (WithName "o" (Type OctetStringType)) (OctetStringValue [(B1,B1,B1,B1,B0,B0,B0,B0)])] [] []))]}))))))],
         testParse "valueA SEQUENCE OF BOOLEAN ::= { TRUE , FALSE }"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (SequenceOfType (Unnamed (Type BooleanType))), assignmentValue=SequenceOfValue [BooleanValue True, BooleanValue False]})],
         testParse "valueA ENUMERATED { a , b } ::= a"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (EnumeratedType [UnnumberedEnumerationEntry "a", UnnumberedEnumerationEntry "b"] Nothing), assignmentValue=EnumeratedValue "a"})],
         testParse "valueA SEQUENCE { b BOOLEAN, c CHOICE { b BOOLEAN } } ::= { b FALSE , c b : TRUE }"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (SequenceType [Required (WithName "b" (Type BooleanType)), Required (WithName "c" (Type (ChoiceType {choices=[WithName "b" (Type BooleanType)]})))] [] []), assignmentValue=SequenceValue [WithName "b" (BooleanValue False), WithName "c" (ChoiceValue {chosen="b", choiceValue=BooleanValue True})]})],
         testParse "valueA NULL ::= NULL"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type NullType, assignmentValue=NullValue})],
         testParse "TypeA ::= ENUMERATED { a, b , ... }"
                   [Assignment (TypeAssignment "TypeA" (Type (EnumeratedType [UnnumberedEnumerationEntry "a", UnnumberedEnumerationEntry "b"] (Just []))))],
         testParse "TypeA ::= ENUMERATED { a, ... , b }"
                   [Assignment (TypeAssignment "TypeA" (Type (EnumeratedType [UnnumberedEnumerationEntry "a"] (Just [UnnumberedEnumerationEntry "b"]))))],
         testParse "valueA INTEGER { five(5) } ::= five"
                   [Assignment (ValueAssignment {name="valueA", asn1Type=Type (IntegerType {namedIntegerValues = Just [WithName "five" 5]}), assignmentValue=IntegerValue 5})]
        ] ++ lexerTests

main = foldr (>>) (putStrLn "OK") tests
}