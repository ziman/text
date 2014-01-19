module Data.Text.ByteString

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

unconsBS : ByteString -> Maybe (Bits 8, ByteString)
unconsBS bs with (strM bs)
  unconsBS ""             | StrNil       = Nothing
  unconsBS (strCons x xs) | StrCons x xs = Just (intToBits . cast . ord $ x, xs)

-- todo: prove totality
%assert_total
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
