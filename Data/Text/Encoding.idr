module Data.Text.Encoding

%access public

CodePoint : Type
CodePoint = Int

-- As defined by Unicode.
replacementChar : CodePoint
replacementChar = 0xFFFD

ByteString : Type
ByteString = String

emptyBS : ByteString
emptyBS = ""

record Encoding : Type where
  Enc :
       -- unconsE returns the first code point
       -- and the predecessor of the number of bytes
       -- to skip to get past the first code point.
       --
       -- In other words, returning (c, n)
       -- will make the decoder skip (S n) bytes.
       (peekE   : ByteString -> Maybe (CodePoint, Nat))
    -> (encodeE : CodePoint -> ByteString)
    -> Encoding

record EncodedString : Encoding -> Type where
  EncS :
    (getBytes_ : ByteString)
    -> EncodedString e

getBytes : EncodedString e -> ByteString
getBytes (EncS bs) = bs

private
dropS : Nat -> ByteString -> ByteString
dropS    Z  bytes = bytes
dropS (S n) bytes with (strM bytes)
  dropS (S n) ""             | StrNil       = ""
  dropS (S n) (strCons x xs) | StrCons x xs = dropS n xs

head : {e : Encoding} -> EncodedString e -> Maybe CodePoint
head {e = Enc pE eE} = map fst . pE . getBytes

cons : {e : Encoding} -> CodePoint -> EncodedString e -> EncodedString e
cons {e = Enc pE eE} c (EncS bs) = EncS (eE c ++ bs)

uncons : {e : Encoding} -> EncodedString e -> Maybe (CodePoint, EncodedString e)
uncons {e = Enc pE eE} (EncS bs) with (pE bs)
  | Just (c, skip) = Just (c, EncS $ dropS skip bs)
  | Nothing        = Nothing

private total
unpack' : (ByteString -> Maybe (CodePoint, Nat)) -> Nat -> Nat -> ByteString -> List CodePoint
unpack' pE    Z     Z  bytes = []
unpack' pE (S n)    Z  bytes = []
unpack' pE (S n) (S l) bytes = unpack' pE n l bytes
unpack' pE    Z  (S l) bytes with (pE bytes)
  | Nothing        = []
  | Just (c, skip) = c :: unpack' pE skip l bytes

unpack : {e : Encoding} -> EncodedString e -> List CodePoint
unpack {e = Enc pE _} (EncS bytes) = unpack' pE 0 (length bytes) bytes

pack : {e : Encoding} -> List CodePoint -> EncodedString e
pack {e = Enc _ eE} = EncS . foldr ((++) . eE) emptyBS

instance Cast (EncodedString e) (EncodedString e') where
  cast = pack . unpack
