module BasicTypes where
import qualified Data.Map

data Bit = B0 | B1 deriving (Show, Eq)
data Hex = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | HA | HB | HC | HD | HE | HF deriving (Show, Eq)
type Octet = (Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit)

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

makeOctet :: [Bit] -> Octet
makeOctet (a:b:c:d:e:f:g:h:[]) = (a,b,c,d,e,f,g,h)

bitsToOctets :: [Bit] -> [Octet]
bitsToOctets bits = let (eight, rest) = splitAt 8 bits in
                    case rest of [] -> [makeOctet (take 8 (bits ++ repeat B0))]
                                 otherwise -> makeOctet eight : bitsToOctets rest