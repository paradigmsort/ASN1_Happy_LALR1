module ASN1Types where
import BasicTypes
import ASN1Lexer

class DoubleFunctor df where
    dfmap :: (a -> c) -> (b -> d) -> df a b -> df c d

class TripleFunctor tf where
    tfmap :: (a -> d) -> (b -> e) -> (c -> f) -> tf a b c -> tf d e f

data ASN1WithName a = WithName String a deriving (Show, Eq)

instance Functor ASN1WithName where
  fmap f (WithName n x) = WithName n (f x)

stripName :: ASN1WithName a -> a
stripName (WithName _ a) = a

isNamed :: String -> ASN1WithName a -> Bool
isNamed name (WithName named _) = name == named

data ASN1OptionallyNamed a = Unnamed a
                           | Named (ASN1WithName a) deriving (Show, Eq)

instance Functor ASN1OptionallyNamed where
  fmap f (Unnamed x) = Unnamed (f x)
  fmap f (Named x) = Named (fmap f x)

fromOptionallyNamed :: ASN1OptionallyNamed a -> a
fromOptionallyNamed (Unnamed a) = a
fromOptionallyNamed (Named (WithName n a)) = a

data ASN1BuiltinOrReference a = Builtin a 
                              | Reference String deriving (Show, Eq)

resolveReference :: (x -> String -> ASN1BuiltinOrReference a) -> x -> ASN1BuiltinOrReference a -> a
resolveReference _ _ (Builtin a) = a
resolveReference f x (Reference r) = resolveReference f x (f x r)

data ASN1RequiredOptionalOrDefault a b = Required a
                                       | Optional a 
                                       | Default a b deriving (Show, Eq)

instance DoubleFunctor ASN1RequiredOptionalOrDefault where
  dfmap f _ (Required a) = Required (f a)
  dfmap f _ (Optional a) = Optional (f a)
  dfmap f g (Default a b) = Default (f a) (g b)

stripROD :: ASN1RequiredOptionalOrDefault a b -> a
stripROD (Required a) =  a
stripROD (Optional a) =  a
stripROD (Default a b) = a

data ASN1StagedAssignment a b = TypeAssignment { name :: String, asn1Type::a }
                              | ValueAssignment { name :: String, asn1Type::a, assignmentValue::b } deriving (Show, Eq)

typeFromAssignment :: ASN1StagedAssignment a b -> a
typeFromAssignment (TypeAssignment _ t) = t
typeFromAssignment (ValueAssignment _ t _) = t

instance DoubleFunctor ASN1StagedAssignment where
  dfmap f _ (TypeAssignment name a) = TypeAssignment name (f a)
  dfmap f g (ValueAssignment name a b) = ValueAssignment name (f a) (g b)

newtype ASN1AssignmentTypeRefUnparsedValue = TypeRefUnparsedValue (ASN1StagedAssignment (ASN1BuiltinOrReference ASN1TypeWithRef) [ASN1Token]) deriving (Show, Eq)
newtype ASN1AssignmentUnparsedValue = UnparsedValue (ASN1StagedAssignment ASN1TypeNoRef [ASN1Token]) deriving (Show, Eq)
newtype ASN1AssignmentIntRef = IntRef (ASN1StagedAssignment ASN1TypeValueParsed ASN1ParsedValue) deriving (Show, Eq)
newtype ASN1Assignment = Assignment (ASN1StagedAssignment ASN1Type ASN1Value) deriving (Show, Eq)

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
                                           preExtensionComponents :: [ASN1RequiredOptionalOrDefault (ASN1WithName a ) b],
                                           extensonAdditions :: [[ASN1RequiredOptionalOrDefault (ASN1WithName a ) b]],
                                           postExtensionComponents :: [ASN1RequiredOptionalOrDefault (ASN1WithName a ) b]
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