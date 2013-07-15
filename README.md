# ASN1_Happy_LALR1

An ASN1 specification parser in Haskell using Happy.
Mostly a learning exercise.
Refer to ITU-T X.680
Licensed under the MIT license (see LICENSE file).

The following constructs are supported:

INTEGER (optional named values)
BOOLEAN
SEQUENCE
SEQUENCE OF
CHOICE
ENUMERATED (extendable)
BIT STRING (optional named values)
OCTET STRING
DEFAULT
OPTIONAL

TODO:
Value references
VisibleString
IA5String
UTCTime
SIZE constraints
