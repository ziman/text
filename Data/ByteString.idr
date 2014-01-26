module Data.ByteString

import Data.Bits

%access public
%default total

abstract
ByteString : Type
ByteString = String

emptyBS : ByteString
emptyBS = ""

nullBS : ByteString -> Bool
nullBS bs with (strM bs)
  nullBS ""             | StrNil       = True
  nullBS (strCons x xs) | StrCons x xs = False

ord8 : Char -> Bits 8
ord8 = intToBits . cast . ord

chr8 : Bits 8 -> Char
chr8 = chr . fromInteger . bitsToInt

consBS : Bits 8 -> ByteString -> ByteString
consBS c bs = strCons (chr8 c) bs

unconsBS : ByteString -> Maybe (Bits 8, ByteString)
unconsBS bs with (strM bs)
  unconsBS ""             | StrNil       = Nothing
  unconsBS (strCons x xs) | StrCons x xs = Just (ord8 x, xs)

-- todo: this could be way more efficient with memcpy()
takeBS : Nat -> ByteString -> ByteString
takeBS    Z  bs = emptyBS
takeBS (S n) bs with (unconsBS bs)
  | Nothing      = ""
  | Just (x, xs) = x `consBS` takeBS n bs

-- todo: this could be more efficient with unsafePointerArithmetic#
dropBS : Nat -> ByteString -> ByteString
dropBS    Z  bs = bs
dropBS (S n) bs with (unconsBS bs)
  | Nothing      = ""
  | Just (x, xs) = dropBS n xs

-- todo: prove totality
%assert_total
spanLength : (Bits 8 -> Bool) -> ByteString -> Nat
spanLength p bs with (unconsBS bs)
  | Nothing = Z
  | Just (x, xs) = if p x then S (spanLength p xs) else Z

appendBS : ByteString -> ByteString -> ByteString
appendBS = (++)

lengthBS : ByteString -> Nat
lengthBS = length

fromString : String -> ByteString
fromString = id

toString : ByteString -> String
toString = id

packBS : List (Bits 8) -> ByteString
packBS = Prelude.Strings.pack . map chr8

unpackBS : ByteString -> List (Bits 8)
unpackBS = map ord8 . Prelude.Strings.unpack
