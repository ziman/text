module Data.Text

import Data.Text.CodePoint
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

%access public
%default total

abstract
record EncodedString : Encoding -> Type where
  EncS :
    (getBytes_ : ByteString)
    -> EncodedString e

-- Required because the autogenerated projection is not very useful.
getBytes : EncodedString e -> ByteString
getBytes (EncS bs) = bs

-- We define Text to be UTF8-encoded packed strings.
Text : Type
Text = EncodedString UTF8

-- Text literals are created by the function Data.Text.str:
--
-- myConst : Text
-- myConst = str "Hello world!"

-- Meant to be used infix: ("bytes" `asEncodedIn` UTF8).
-- It is up to the user to ensure that the ByteString has the right encoding.
asEncodedIn : ByteString -> (e : Encoding) -> EncodedString e
asEncodedIn bs e = EncS bs

-- A shortcut for the most common use case.
fromUTF8 : ByteString -> Text
fromUTF8 s = s `asEncodedIn` UTF8

instance Eq (EncodedString e) where
  (==) (EncS x) (EncS y) = x == y

instance Ord (EncodedString e) where
  compare (EncS x) (EncS y) = compare x y

instance Show Text where
  show = toString . getBytes

private
foldl' :
  (ByteString -> Maybe (CodePoint, Nat))  -- The peek function
  -> (Nat -> a -> CodePoint -> a)  -- The folding function, first arg = codepoint bytes
  -> a            -- The seed value
  -> Nat          -- Skip this number of bytes first
  -> Nat          -- Total string length
  -> ByteString   -- The bytes
  -> a
foldl' pE f z    Z     Z  bs = z
foldl' pE f z (S n)    Z  bs = z

foldl' pE f z (S n) (S l) bs with (unconsBS bs)  -- skip step
  | Nothing      = z
  | Just (x, xs) = foldl' pE f z n l xs

foldl' pE f z    Z  (S l) bs =
  case pE bs of
    Nothing        => z
    Just (c, skip) => case unconsBS bs of
      Nothing      => z
      Just (x, xs) => foldl' pE f (f skip z c) skip l xs

-- TODO: check that this function really returns early
private
foldr' :
  (ByteString -> Maybe (CodePoint, Nat))  -- The peek function
  -> (Nat -> CodePoint -> a -> a)  -- The folding function, first arg = codepoint bytes
  -> a            -- The seed value
  -> Nat          -- Skip this number of bytes first
  -> Nat          -- Total string length
  -> ByteString   -- The bytes
  -> a
foldr' pE f z    Z     Z  bs = z
foldr' pE f z (S n)    Z  bs = z

foldr' pE f z (S n) (S l) bs with (unconsBS bs)  -- skip step
  | Nothing      = z
  | Just (x, xs) = foldr' pE f z n l xs

foldr' pE f z    Z  (S l) bs =
  case pE bs of
    Nothing        => z
    Just (c, skip) => f skip c (lazy (case unconsBS bs of
      Nothing      => z
      Just (x, xs) => foldr' pE f z skip l xs))

foldr : {e : Encoding} -> (CodePoint -> a -> a) -> a -> EncodedString e -> a
foldr {e = Enc pE _} f z (EncS bs) = foldr' pE (const f) z 0 (lengthBS bs) bs

foldl : {e : Encoding} -> (a -> CodePoint -> a) -> a -> EncodedString e -> a
foldl {e = Enc pE _} f z (EncS bs) = foldl' pE (const f) z 0 (lengthBS bs) bs

-- Will overflow stack on long texts. Use reverse . foldl to avoid that.
unpack : {e : Encoding} -> EncodedString e -> List CodePoint
unpack {e = Enc pE _} = Data.Text.foldr (::) []

pack : {e : Encoding} -> List CodePoint -> EncodedString e
pack {e = Enc _ eE} = EncS . foldr (appendBS . eE) emptyBS

-- O(n). Convert a (possibly wide-char) String to Text.
-- Intended for Text literals.
str : String -> Text
str = pack . map fromChar . unpack

-- O(1). Construct a single-char encoded string.
singleton : {e : Encoding} -> CodePoint -> EncodedString e
singleton {e = Enc _ eE} c = EncS (eE c)

-- O(1). Construct an empty encoded string.
empty : EncodedString e
empty = EncS emptyBS

-- O(n). Prepend a single character.
cons : {e : Encoding} -> CodePoint -> EncodedString e -> EncodedString e
cons {e = Enc pE eE} c (EncS bs) = EncS (eE c `appendBS` bs)

-- O(1). Uncons the first character or return Nothing if the string is empty.
uncons : {e : Encoding} -> EncodedString e -> Maybe (CodePoint, EncodedString e)
uncons {e = Enc pE eE} (EncS bs) with (pE bs)
  | Just (c, skip) = Just (c, EncS $ dropBS skip bs)
  | Nothing        = Nothing

-- O(n). Append a single character.
snoc : {e : Encoding} -> CodePoint -> EncodedString e -> EncodedString e
snoc {e = Enc pE eE} c (EncS bs) = EncS (bs `appendBS` eE c)

-- O(1). Get the first character or Nothing if the string is empty.
head : {e : Encoding} -> EncodedString e -> Maybe CodePoint
head {e = Enc pE eE} = map fst . pE . getBytes

-- O(1). Get the tail of the string or Nothing if the string is empty.
tail : {e : Encoding} -> EncodedString e -> Maybe (EncodedString e)
tail {e = Enc pE eE} = map snd . uncons

-- O(n). Get the length of the string. (Count all codepoints.)
length : EncodedString e -> Nat
length = Data.Text.foldl (\n => \_ => S n) Z

-- O(n_left). Concatenate two strings.
append : EncodedString e -> EncodedString e -> EncodedString e
append (EncS s) (EncS s') = EncS (s `appendBS` s')

(++) : EncodedString e -> EncodedString e -> EncodedString e
(++) = append

instance Semigroup (EncodedString e) where
  (<+>) = append

instance Monoid (EncodedString e) where
  neutral = empty

-- init is unsupported
-- last is unsupported

-- O(1). Determines whether the string is empty.
null : EncodedString e -> Bool
null (EncS bs) = nullBS bs

-- O(n). Will overflow the stack for very long strings.
map : (CodePoint -> CodePoint) -> EncodedString e -> EncodedString e'
map {e' = Enc _ eE'} f = foldr (cons . f) empty

instance Cast (EncodedString e) (EncodedString e') where
  cast = map id

-- O(n). Concatenate with separators.
intercalate : EncodedString e -> List (EncodedString e) -> EncodedString e
intercalate sep []        = empty
intercalate sep [x]       = x
intercalate sep (x :: xs) = x `append` (sep `append` intercalate sep xs)

-- O(n). Rearrange the codepoints in the opposite order.
-- TODO: is it correct to do this in Unicode?
reverse : EncodedString e -> EncodedString e
reverse = foldl (flip cons) empty

-- O(n). Concatenate all strings in the list.
concat : List (EncodedString e) -> EncodedString e
concat [] = empty
concat (x :: xs) = x `append` concat xs

-- O(n). Concatenate all strings resulting from a codepoint mapping.
concatMap : (CodePoint -> EncodedString e) -> EncodedString e -> EncodedString e
concatMap f = foldr (append . f) empty

-- O(n). Check whether any codepoint has the specified property.
-- Will overflow the stack on very long texts
-- but will exit early if the desired codepoint is found.
any : (CodePoint -> Bool) -> EncodedString e -> Bool
any p = foldr ((||) . p) False

-- O(n). Check whether all codepoints have the specified property.
all : (CodePoint -> Bool) -> EncodedString e -> Bool
all p = foldl (\r => \c => r && p c) True

-- O(n*|w|). Repeat the string n times.
replicate : Nat -> EncodedString e -> EncodedString e
replicate    Z  s = empty
replicate (S n) s = s `append` replicate n s

private
spanByteLength : {e : Encoding} -> (CodePoint -> Bool) -> EncodedString e -> Nat
spanByteLength {e = Enc pE _} p (EncS bs) = foldr' pE f Z Z (lengthBS bs) bs
  where
    f : Nat -> CodePoint -> Nat -> Nat
    f nbytes cp rest = nbytes + rest

private
takeBytes : Nat -> EncodedString e -> EncodedString e
takeBytes n (EncS bs) = EncS (takeBS n bs)

private
dropBytes : Nat -> EncodedString e -> EncodedString e
dropBytes n (EncS bs) = EncS (dropBS n bs)

-- O(n). Return the longest prefix of code points that satisfy the predicate.
takeWhile : (CodePoint -> Bool) -> EncodedString e -> EncodedString e
takeWhile p s = spanByteLength p s `takeBytes` s

-- O(n). Drop the longest prefix of code points that satisfy the predicate.
dropWhile : (CodePoint -> Bool) -> EncodedString e -> EncodedString e
dropWhile p s = spanByteLength p s `dropBytes` s

-- O(n). Slightly more efficient equivalent to (takeWhile p, dropWhile p)
span : (CodePoint -> Bool) -> EncodedString e -> (EncodedString e, EncodedString e)
span p s = let n = spanByteLength p s in (takeBytes n s, dropBytes n s)

-- O(n). Defined as span (not . p).
break : (CodePoint -> Bool) -> EncodedString e -> (EncodedString e, EncodedString e)
break p = span (not . p)

-- O(n). (foldr-)Traverse all spans of code points satisfying the predicate.
%assert_total
foldSpans : (CodePoint -> Bool) -> (EncodedString e -> a -> a) -> a -> EncodedString e -> a
foldSpans p f z s with (null s)
  | True  = z
  | False =
      let (s, rest) = span p s in
        f s (lazy (foldSpans p f z $ dropWhile (not . p) rest))

-- O(n). Count all spans of code points satisfying the predicate.
spanCount : (CodePoint -> Bool) -> EncodedString e -> Nat
spanCount p = foldSpans p (const S) Z

-- O(n). Split into spans separated by code points satisfying the predicate, discarding the separators.
split : (sep : CodePoint -> Bool) -> EncodedString e -> List (EncodedString e)
split sep = foldSpans (not . sep) (::) []

-- O(n). Split to lines, according to Data.Text.CodePoint.isNewline.
lines : EncodedString e -> List (EncodedString e)
lines = split isNewline

-- O(n). Count lines, according to Data.Text.CodePoint.isNewline.
lineCount : EncodedString e -> Nat
lineCount = spanCount (not . isNewline)

-- O(n). Split to words, according to Data.Text.CodePoint.isSpace.
words : EncodedString e -> List (EncodedString e)
words = split isSpace

-- O(n). Count words, according to Data.Text.CodePoint.isSpace.
wordCount : EncodedString e -> Nat
wordCount = spanCount (not . isSpace)

private
cpointsToBytes : (pE : ByteString -> Maybe (CodePoint, Nat)) -> Nat -> Nat -> ByteString -> Nat
cpointsToBytes pE    Z  k bs = k
cpointsToBytes pE (S n) k bs with (pE bs)
  | Nothing = k
  | Just (c, nbytes) = cpointsToBytes pE n (nbytes + k) (dropBS nbytes bs)

-- O(n). Extract the first N code points.
take : {e : Encoding} -> Nat -> EncodedString e -> EncodedString e
take {e = Enc pE _} n (EncS bs) = EncS (takeBS (cpointsToBytes pE n 0 bs) bs)

-- O(n). Drop the first N code points.
drop : {e : Encoding} -> Nat -> EncodedString e -> EncodedString e
drop {e = Enc pE _} n (EncS bs) = EncS (dropBS (cpointsToBytes pE n 0 bs) bs)

-- O(n). Split after the N-th code point.
splitAt : {e : Encoding} -> Nat -> EncodedString e -> (EncodedString e, EncodedString e)
splitAt {e = Enc pE _} n (EncS bs) = (EncS $ takeBS nbytes bs, EncS $ dropBS nbytes bs)
  where
    nbytes = cpointsToBytes pE n 0 bs
