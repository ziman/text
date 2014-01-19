module Data.Text.Encoding.UTF8

import Data.Bits
import Data.Text.Encoding

%access private

-- As defined by Unicode.
replacementChar : CodePoint
replacementChar = intToBits 0xFFFD

x0x3F : Bits 32
x0x3F = intToBits 0x3F

x0x06 : Bits 32
x0x06 = intToBits 0x06

x0x00 : Bits 32
x0x00 = intToBits 0x00

isCont : Int -> Bool
isCont c = (c >= 0x80) && (c < 0xC0)  -- todo: improve by using bit ops?

countCont : ByteString -> Nat
countCont = Prelude.Strings.length . fst . span (isCont . ord)

i2b : Int -> Bits 32
i2b = intToBits . cast

cont : Bits 32 -> Nat -> ByteString -> Maybe (Bits 32)
cont k    Z  bs = Just k
cont k (S n) bs with (strM bs)
  cont k (S n) ""             | StrNil       = Nothing
  cont k (S n) (strCons x xs) | StrCons x xs =
    let x' = ord x in
      if isCont x'
        then cont ((k `shiftLeft` x0x06) `or` (i2b x' `and` x0x3F)) n xs
        else Nothing

overlong : Nat -> Bits 32 -> Bool
overlong n x = x > intToBits (maxRepresentable n)
  where
    maxRepresentable : Nat -> Integer
    maxRepresentable          Z    = 0x00007F
    maxRepresentable       (S Z)   = 0x0007FF
    maxRepresentable    (S (S Z))  = 0x00FFFF
    maxRepresentable (S (S (S _))) = 0x1FFFFF

decode : Nat -> Int -> ByteString -> CodePoint
decode conts first bs with (cont x0x00 conts bs)
  | Nothing = replacementChar
  | Just c  =
     let val = (i2b first `shiftLeft` i2b (6 * cast conts)) `or` c
     in if overlong conts val
          then replacementChar
          else val

public
peek : ByteString -> Maybe (CodePoint, Nat)
peek bs with (strM bs)
  peek ""             | StrNil       = Nothing
  peek (strCons x xs) | StrCons x xs =
    let x' = ord x in
      if x' < 0x80
        then Just (i2b x', 0)
        else if x' < 0xC0
          then Just (replacementChar, countCont xs)
          else if x' < 0xE0
            then Just (decode 1 x' xs, 1)
            else if x' < 0xF0
              then Just (decode 2 x' xs, 2)
              else if x' < 0xF8
                then Just (decode 3 x' xs, 3)
                else Just (replacementChar, countCont xs)

encode : CodePoint -> ByteString
encode c = ""

public
UTF8 : Encoding
UTF8 = Enc peek encode
