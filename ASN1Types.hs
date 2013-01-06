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

data ASN1StagedAssignment a b = StagedAssignment (ASN1WithName (ASN1StagedAssigned a b)) deriving (Show, Eq)
instance DoubleFunctor ASN1StagedAssignment where
  dfmap f g (StagedAssignment x) = StagedAssignment (fmap (dfmap f g) x)

data ASN1StagedAssigned a b = TypeAssignment { asn1Type::a }
                            | ValueAssignment { asn1Type::a, assignmentValue::b } deriving (Show, Eq)

typeFromAssigned :: ASN1StagedAssigned a b -> a
typeFromAssigned (TypeAssignment t) = t
typeFromAssigned (ValueAssignment t _) = t

isTypeAssigned :: ASN1StagedAssigned a b -> Bool
isTypeAssigned (TypeAssignment _) = True
isTypeAssigned (ValueAssignment _ _) = False

instance DoubleFunctor ASN1StagedAssigned where
  dfmap f _ (TypeAssignment a) = TypeAssignment (f a)
  dfmap f g (ValueAssignment a b) = ValueAssignment (f a) (g b)

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

instance TripleFunctor ASN1StagedType where
  tfmap _ _ h (BitStringType namedBits) = BitStringType (fmap (map (fmap h)) namedBits)
  tfmap _ _ _ BooleanType = BooleanType
  tfmap f _ _ (ChoiceType choices) = ChoiceType (map (fmap f) choices)
  tfmap _ _ _ (EnumeratedType entry ext) = EnumeratedType entry ext
  tfmap _ _ h (IntegerType namedIntegerValues) = IntegerType (fmap (map (fmap h)) namedIntegerValues)
  tfmap _ _ _ NullType = NullType
  tfmap _ _ _ OctetStringType = OctetStringType
  tfmap f g _ (SequenceType pre ext post) = SequenceType (map (dfmap (fmap f) g) pre)
                                                         (map (map (dfmap (fmap f) g)) ext)
                                                         (map (dfmap (fmap f) g) post)
  tfmap f _ _ (SequenceOfType stype) = SequenceOfType (fmap f stype)

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

instance Functor ASN1StagedValue where
  fmap f (BitStringValue bits) = BitStringValue bits
  fmap f (BooleanValue b) = BooleanValue b
  fmap f (ChoiceValue chosen choiceValue) = ChoiceValue chosen (fmap f choiceValue)
  fmap f (EnumeratedValue s) = EnumeratedValue s
  fmap f (IntegerValue intVal) = IntegerValue (f intVal)
  fmap f NullValue = NullValue
  fmap f (OctetStringValue octets) = OctetStringValue octets
  fmap f (SequenceValue namedValues) = SequenceValue (map (fmap (fmap f)) namedValues)
  fmap f (SequenceOfValue values) = SequenceOfValue (map (fmap f) values)

type ASN1ParsedValue = ASN1StagedValue (ASN1BuiltinOrReference Integer)
type ASN1Value = ASN1StagedValue Integer