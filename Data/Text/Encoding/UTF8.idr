module Data.Text.Encoding.UTF8

import Data.Bits
import Data.Bytes
import Data.ByteArray
import Data.Text.CodePoint
import Data.Text.Encoding

%access private
%default total

i2b : Int -> Bits 21
i2b = intToBits . cast

fromByte : Bits8 -> Bits 8
fromByte = MkBits

toByte : Bits 8 -> Bits8
toByte (MkBits byte) = byte

||| Determines whether the given byte is a continuation byte.
isCont : Bits 8 -> Bool
isCont c = (c `and` intToBits 0xC0) == intToBits 0x80

||| Returns the number of leading continuation bytes.
contCount : Bytes -> Nat
contCount = cast . spanLength (isCont . fromByte)

||| Returns the payload bits from the leading continuation bytes.
cont : Bits 21 -> Nat -> Bytes -> Maybe (Bits 21)
cont k    Z  bs = Just k
cont k (S n) bs with (consView bs)
  | Nil       = Nothing
  | Cons x xs =
      if isCont (fromByte x)
        then cont ((k `shiftLeft` intToBits 0x06) `or` zeroExtend (fromByte x `and` intToBits 0x3F)) n xs
        else Nothing

||| Determines whether a code is overlong for its continuation byte count.
overlong : Nat -> Bits 21 -> Bool
overlong n x = x < intToBits (minRepresentable n)
  where
    minRepresentable : Nat -> Integer
    minRepresentable          Z    = 0x00000
    minRepresentable       (S Z)   = 0x00080
    minRepresentable    (S (S Z))  = 0x00800
    minRepresentable (S (S (S _))) = 0x10000

||| Payload mask for the first code byte, depending
||| on the number of continuation bytes.
firstMask : Nat -> Bits 8
firstMask          Z    = intToBits 0x7F
firstMask       (S Z)   = intToBits 0x1F
firstMask    (S (S Z))  = intToBits 0x0F
firstMask (S (S (S _))) = intToBits 0x07

||| Decode a multi-byte codepoint.
decode : Nat -> Bits 8 -> Bytes -> CodePoint
decode conts first bs with (cont (intToBits 0) conts bs)
  | Nothing = replacementChar
  | Just c  =
     let val = (zeroExtend (first `and` firstMask conts) `shiftLeft` i2b (6 * cast conts)) `or` c
     in if overlong conts val
          then replacementChar
          else fromBits val

peek : Bytes -> Maybe (CodePoint, Nat)
peek bs with (consView bs)
  | Nil = Nothing
  | Cons xb xs = let x = fromByte xb in
      if x < intToBits 0x80
        then Just (fromBits $ zeroExtend x, 0)
        else if x < intToBits 0xC0
          then Just (replacementChar, contCount xs)
          else if x < intToBits 0xE0
            then Just (decode 1 x xs, 1)
            else if x < intToBits 0xF0
              then Just (decode 2 x xs, 2)
              else if x < intToBits 0xF8
                then Just (decode 3 x xs, 3)
                else Just (replacementChar, contCount xs)

trunc : Bits 21 -> Byte
trunc = toByte . truncate {m = 13}

encode : CodePoint -> Bytes
encode cp =
    if c <= intToBits 0x7F
      then empty |> trunc c
      else if c <= intToBits 0x7FF
        then   enc ((c `shr`  6) `ori` 0xC0) [c]
        else if c <= intToBits 0xFFFF
          then enc ((c `shr` 12) `ori` 0xE0) [c `shr` 6, c]
          else enc ((c `shr` 18) `ori` 0xF0) [c `shr` 12, c `shr` 6, c]
  where
    c : Bits 21
    c = toBits cp

    shr : Bits 21 -> Int -> Bits 21
    shr x n = x `shiftRightLogical` i2b n

    ori : Bits 21 -> Int -> Bits 21
    ori x i = x `or` i2b i

    -- ideally, we would like to inline this
    cont : Bytes -> List (Bits 21) -> Bytes
    cont bs []        = bs
    cont bs (x :: xs) = cont (bs `snoc` trunc ((x `and` intToBits 0x3F) `or` intToBits 0x80)) xs

    enc : Bits 21 -> List (Bits 21) -> Bytes
    enc b cs = (empty |> trunc b) `cont` cs

export
UTF8 : Encoding
UTF8 = Enc peek encode
