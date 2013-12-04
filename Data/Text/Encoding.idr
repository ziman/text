module Data.Text.Encoding

%access public

CodePoint : Type
CodePoint = Int

ByteString : Type
ByteString = String

emptyBS : ByteString
emptyBS = ""

record Encoding : Type where
  Enc :
    (unconsE : ByteString -> Maybe (CodePoint, ByteString))
    -> (consE : CodePoint -> ByteString -> ByteString)
    -> Encoding

record EncodedString : Encoding -> Type where
  EncS :
    (getBytes_ : ByteString)
    -> EncodedString e

getBytes : EncodedString e -> ByteString
getBytes (EncS bs) = bs

uncons : {e : Encoding} -> EncodedString e -> Maybe (CodePoint, EncodedString e)
uncons {e = Enc uE cE} (EncS bs) with (uE bs)
  | Just (c, rest) = Just (c, EncS rest)
  | Nothing        = Nothing

cons : {e : Encoding} -> CodePoint -> EncodedString e -> EncodedString e
cons {e = Enc uE cE} c (EncS bs) = EncS (cE c bs)

private
unfoldr : (a -> Maybe (b, a)) -> a -> List b
unfoldr f x with (f x)
  | Just (y, x') = y :: unfoldr f x'
  | Nothing      = []

unpack : {e : Encoding} -> EncodedString e -> List CodePoint
unpack {e = Enc u _} = unfoldr u . getBytes

pack : {e : Encoding} -> List CodePoint -> EncodedString e
pack {e = Enc _ c} = EncS . foldr c emptyBS

instance Cast (EncodedString e) (EncodedString e') where
  cast = pack . unpack
