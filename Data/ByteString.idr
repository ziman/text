module Data.ByteString

import Data.Bits

%access public
%default total

abstract
record ByteString : Type where
  BS : (toString_ : String) -> ByteString

instance Eq ByteString where
  (==) (BS x) (BS y) = x == y

instance Ord ByteString where
  compare (BS x) (BS y) = compare x y

emptyBS : ByteString
emptyBS = BS ""

nullBS : ByteString -> Bool
nullBS (BS bs) with (strM bs)
  nullBS (BS "")             | StrNil       = True
  nullBS (BS (strCons x xs)) | StrCons x xs = False

ord8 : Char -> Bits 8
ord8 = intToBits . cast . ord

chr8 : Bits 8 -> Char
chr8 = chr . fromInteger . bitsToInt

consBS : Bits 8 -> ByteString -> ByteString
consBS c (BS bs) = BS (strCons (chr8 c) bs)

unconsBS : ByteString -> Maybe (Bits 8, ByteString)
unconsBS (BS bs) with (strM bs)
  unconsBS (BS "")             | StrNil       = Nothing
  unconsBS (BS (strCons x xs)) | StrCons x xs = Just (ord8 x, BS xs)

-- todo: this could be way more efficient with memcpy()
takeBS : Nat -> ByteString -> ByteString
takeBS    Z  bs = emptyBS
takeBS (S n) bs with (unconsBS bs)
  | Nothing      = emptyBS
  | Just (x, xs) = x `consBS` takeBS n bs

-- todo: this could be more efficient with unsafePointerArithmetic#
dropBS : Nat -> ByteString -> ByteString
dropBS    Z  bs = bs
dropBS (S n) bs with (unconsBS bs)
  | Nothing      = emptyBS
  | Just (x, xs) = dropBS n xs

-- todo: prove totality
%assert_total
spanLength : (Bits 8 -> Bool) -> ByteString -> Nat
spanLength p bs with (unconsBS bs)
  | Nothing = Z
  | Just (x, xs) = if p x then S (spanLength p xs) else Z

appendBS : ByteString -> ByteString -> ByteString
appendBS (BS x) (BS y) = BS (x ++ y)

fromString : String -> ByteString
fromString = BS

toString : ByteString -> String
toString (BS bs) = bs

lengthBS : ByteString -> Nat
lengthBS = Prelude.Strings.length . toString

packBS : List (Bits 8) -> ByteString
packBS = fromString . Prelude.Strings.pack . map chr8

unpackBS : ByteString -> List (Bits 8)
unpackBS = map ord8 . Prelude.Strings.unpack . toString
