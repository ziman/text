module Data.Text.CodePoint

import Data.Bits
import Data.ByteString

%access public
%default total

abstract
record CodePoint : Type where
  CP : (toBits_ : Bits 21) -> CodePoint

instance Eq CodePoint where
  (==) (CP x) (CP y) = x == y

instance Ord CodePoint where
  compare (CP x) (CP y) = compare x y

toBits : CodePoint -> Bits 21
toBits (CP cp) = cp

fromBits : Bits 21 -> CodePoint
fromBits = CP

-- TODO: bleargh
%assert_total
private
showHex : Int -> String -> String
showHex n =
    if n <= 0
      then id
      else showHex (n `div` 16) . strCons (hex (n `mod` 16))
  where
    hex c = if c < 10 then chr (ord '0' + c) else chr (ord 'A' + c)

coerceBits : Bits m -> Bits n
coerceBits = intToBits . bitsToInt

-- TODO: this really ought to be nicer
infixr 3 +++
instance Show CodePoint where
  show (CP x) =
      if (x <= intToBits 0xFF)
        then (show . chr . fromInteger . bitsToInt $ x)
        else ("'\\x" ++ showHex (fromInteger $ bitsToInt x) "'")

replacementChar : CodePoint
replacementChar = CP (intToBits 0xFFFD)

fromChar : Char -> CodePoint
fromChar = CP . zeroExtend . ord8

ord : CodePoint -> Int
ord = fromInteger . bitsToInt . toBits

inRange : List Int -> CodePoint -> Bool
inRange xs cp = ord cp `elem` xs

isNewline : CodePoint -> Bool
isNewline = inRange [0x0A, 0x0D]

isSpace : CodePoint -> Bool
isSpace = inRange [0x0A, 0x0D, 0x20]  -- TODO
