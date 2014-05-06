module Data.ByteString

import Data.Bits
import Data.Buffer

%access public
%default total

abstract
record ByteString : Type where
  BS : (length_ : Nat) -> (toBuffer_ : Buffer length_) -> ByteString

length : ByteString -> Nat
length (BS n _) = n

toBuffer : (bs : ByteString) -> Buffer (length bs)
toBuffer (BS n bs) = bs

empty : ByteString
empty = BS _ (allocate 8)

empty' : (sizeHint : Nat) -> ByteString
empty' szHint = BS _ (allocate szHint)

null : ByteString -> Bool
null (BS    Z  _) = True
null (BS (S _) _) = False

ord8 : Char -> Bits 8
ord8 = intToBits . cast . ord

chr8 : Bits 8 -> Char
chr8 = chr . fromInteger . bitsToInt

snoc : ByteString -> Bits 8 -> ByteString
snoc (BS n bs) (MkBits c) = BS (n + 1) (appendBits8 bs 1 c)

uncons : ByteString -> Maybe (Bits 8, ByteString)
uncons (BS    Z  bs) = Nothing
uncons (BS (S n) bs) = Just (MkBits $ peekBits8 bs fZ, BS n (peekBuffer 1 bs))

private
data Diff : Nat -> Nat -> Type where
  Geq : (n, m : Nat) -> Diff (n + m) m
  Lt  : (n, m : Nat) -> Diff m (S n + m)

private
diff : (m, n : Nat) -> Diff m n
diff    m     Z  = replace (plusZeroRightNeutral {P = \k => Diff k Z} m) $ Geq m Z
diff    Z  (S n) = Lt  n Z
diff (S m) (S n) with (diff m n)
  | Geq p q = ?rhs1
  | Lt  p q = ?rhs2

{-
take : Nat -> ByteString -> ByteString
take n (BS len bs) = 
  -}
{-

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

packBS : List (Bits 8) -> ByteString
packBS = fromString . Prelude.Strings.pack . map chr8

unpackBS : ByteString -> List (Bits 8)
unpackBS = map ord8 . Prelude.Strings.unpack . toString

singletonBS : Bits 8 -> ByteString
singletonBS c = c `consBS` emptyBS
-}

{-
instance Eq ByteString where
  (==) (BS x) (BS y) = x == y

instance Ord ByteString where
  compare (BS x) (BS y) = compare x y

instance Show ByteString where
  show (BS s) = show s
-}
