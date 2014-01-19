module Data.Text.Encoding.UTF8

import Data.Bits
import Data.Text.ByteString
import Data.Text.Encoding

%access private

-- Placeholder for invalid codes.
replacementChar : CodePoint
replacementChar = intToBits 0xFFFD

i2b : Int -> Bits 32
i2b = intToBits . cast

-- Determines whether the given byte is a continuation byte.
isCont : Bits 8 -> Bool
isCont c = (c `and` intToBits 0xC0) == intToBits 0x80

-- Returns the number of leading continuation bytes.
contCount : ByteString -> Nat
contCount = spanLength isCont

-- Returns the payload bits from the leading continuation bytes.
cont : Bits 32 -> Nat -> ByteString -> Maybe (Bits 32)
cont k    Z  bs = Just k
cont k (S n) bs with (unconsBS bs)
  | Nothing       = Nothing
  | Just (x, xs) =
      if isCont x
        then cont ((k `shiftLeft` intToBits 0x06) `or` zeroExtend (x `and` intToBits 0x3F)) n xs
        else Nothing

-- Determines whether a code is overlong for its continuation byte count.
overlong : Nat -> Bits 32 -> Bool
overlong n x = x < intToBits (minRepresentable n)
  where
    minRepresentable : Nat -> Integer
    minRepresentable          Z    = 0x00000
    minRepresentable       (S Z)   = 0x00080
    minRepresentable    (S (S Z))  = 0x00800
    minRepresentable (S (S (S _))) = 0x10000

-- Payload mask for the first code byte, depending
-- on the number of continuation bytes.
firstMask : Nat -> Bits 8
firstMask          Z    = intToBits 0x7F
firstMask       (S Z)   = intToBits 0x1F
firstMask    (S (S Z))  = intToBits 0x0F
firstMask (S (S (S _))) = intToBits 0x07

-- Decode a multi-byte codepoint.
decode : Nat -> Bits 8 -> ByteString -> CodePoint
decode conts first bs with (cont (intToBits 0) conts bs)
  | Nothing = replacementChar
  | Just c  =
     let val = (zeroExtend (first `and` firstMask conts) `shiftLeft` i2b (6 * cast conts)) `or` c
     in if overlong conts val
          then replacementChar
          else val

peek : ByteString -> Maybe (CodePoint, Nat)
peek bs with (unconsBS bs)
  | Nothing  = Nothing
  | Just (x, xs) =
      if x < intToBits 0x80
        then Just (zeroExtend x, 0)
        else if x < intToBits 0xC0
          then Just (replacementChar, contCount xs)
          else if x < intToBits 0xE0
            then Just (decode 1 x xs, 1)
            else if x < intToBits 0xF0
              then Just (decode 2 x xs, 2)
              else if x < intToBits 0xF8
                then Just (decode 3 x xs, 3)
                else Just (replacementChar, contCount xs)

encode : CodePoint -> ByteString
encode c = emptyBS

public
UTF8 : Encoding
UTF8 = Enc peek encode
