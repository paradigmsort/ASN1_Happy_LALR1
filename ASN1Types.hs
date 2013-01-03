module ASN1Types where

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

data ASN1RequiredOptionalOrDefault a b = Required a
                                       | Optional a 
                                       | Default a b deriving (Show, Eq)

class DoubleFunctor df where
    dfmap :: (a -> c) -> (b -> d) -> df a b -> df c d

instance DoubleFunctor ASN1RequiredOptionalOrDefault where
  dfmap f _ (Required a) = Required (f a)
  dfmap f _ (Optional a) = Optional (f a)
  dfmap f g (Default a b) = Default (f a) (g b)

stripROD :: ASN1RequiredOptionalOrDefault a b -> a
stripROD (Required a) =  a
stripROD (Optional a) =  a
stripROD (Default a b) = a