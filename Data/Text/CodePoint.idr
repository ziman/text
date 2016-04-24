module Data.Text.CodePoint

import Data.Bits
import Data.Bytes

%access public export
%default total

record CodePoint where
  constructor CP
  toBits : Bits 21

implementation Eq CodePoint where
  (==) (CP x) (CP y) = x == y

implementation Ord CodePoint where
  compare (CP x) (CP y) = compare x y

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

-- TODO: this really ought to be nicer
infixr 3 +++
implementation Show CodePoint where
  show (CP x) =
      if (x <= intToBits 0xFF)
        then (show . chr . fromInteger . bitsToInt $ x)
        else ("'\\x" ++ showHex (fromInteger $ bitsToInt x) "'")

replacementChar : CodePoint
replacementChar = CP (intToBits 0xFFFD)

-- Attempt to support non-8-bit characters, too.
-- This is used in Data.Text.str,
-- which is intended for Text literals
fromChar : Char -> CodePoint
fromChar = CP . intToBits . cast . Prelude.Chars.ord

ord : CodePoint -> Int
ord = fromInteger . bitsToInt . toBits

inRange : List Int -> CodePoint -> Bool
inRange xs cp = ord cp `elem` xs

mock : (CodePoint -> a) -> (Char -> a) -> CodePoint -> a
mock dflt f c =
  if ord c <= 0xFF
    then f (chr . fromInteger . bitsToInt . toBits $ c)
    else dflt c

isNewline : CodePoint -> Bool
isNewline = inRange [0x0A, 0x0D]

-- TODO:
-- The following functions certainly don't work entirely correctly,
-- they only replicate the behaviour of their ASCII equivalents.

isSpace : CodePoint -> Bool
isSpace = mock (const False) Prelude.Chars.isSpace  -- TODO

isAlpha : CodePoint -> Bool
isAlpha = mock (const False) Prelude.Chars.isAlpha  -- TODO

toLower : CodePoint -> CodePoint
toLower = mock id (fromChar . Prelude.Chars.toLower) -- TODO

toUpper : CodePoint -> CodePoint
toUpper = mock id (fromChar . Prelude.Chars.toUpper) -- TODO
