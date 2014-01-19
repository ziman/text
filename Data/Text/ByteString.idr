module Data.Text.ByteString

import Data.Bits

%access public

abstract
ByteString : Type
ByteString = String

emptyBS : ByteString
emptyBS = ""

unconsBS : ByteString -> Maybe (Bits 8, ByteString)
unconsBS bs with (strM bs)
  unconsBS ""             | StrNil       = Nothing
  unconsBS (strCons x xs) | StrCons x xs = Just (intToBits . cast . ord $ x, xs)

dropBS : Nat -> ByteString -> ByteString
dropBS    Z  bs = bs
dropBS (S n) bs with (unconsBS bs)
  | Nothing      = ""
  | Just (x, xs) = dropBS n xs

spanLength : (Bits 8 -> Bool) -> ByteString -> Nat
spanLength p bs with (unconsBS bs)
  | Nothing = Z
  | Just (x, xs) = if p x then S (spanLength p xs) else Z

catBS : ByteString -> ByteString -> ByteString
catBS = (++)

lengthBS : ByteString -> Nat
lengthBS = length

fromString : String -> ByteString
fromString = id
