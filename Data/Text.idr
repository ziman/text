module Data.Text

import Data.Bytes

import Data.Text.CodePoint
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

%access public export
%default total

||| The type of encoded strings, parameterised by the encoding.
record EncodedString (e : Encoding) where
  constructor EncS
  getBytes : Bytes

||| Text is a UTF8-encoded packed string.
Text : Type
Text = EncodedString UTF8

-- Text literals are created by the function Data.Text.str:
--
-- myConst : Text
-- myConst = str "Hello world!"

||| Interpret the given bytes in the given encoding.
|||
||| Meant to be used infix: ("bytes" `asEncodedIn` UTF8).
||| It is up to the user to ensure that the Bytes has the right encoding.
asEncodedIn : Bytes -> (e : Encoding) -> EncodedString e
asEncodedIn bs e = EncS bs

||| Interpret bytes as UTF8 text.
fromUTF8 : Bytes -> Text
fromUTF8 s = s `asEncodedIn` UTF8

implementation Eq (EncodedString e) where
  (==) (EncS x) (EncS y) = x == y

implementation Ord (EncodedString e) where
  compare (EncS x) (EncS y) = compare x y

-- We could rewrite these folds in a structurally recursive form
-- (in fact, they already were) but this form is much easier to read
-- and also probably more efficient, while keeping totality guarantees.
foldl' :
  (Bytes -> Maybe (CodePoint, Nat))  -- The peek function
  -> (Nat -> a -> CodePoint -> a)  -- The folding function, first arg = codepoint bytes
  -> a            -- The seed value
  -> Bytes   -- The bytes
  -> a
foldl' pE f z bs with (pE bs)
  | Nothing     = z
  | Just (c, n) = foldl' pE f (f (S n) z c) (assert_smaller bs $ dropPrefix (cast $ S n) bs)

-- TODO: check that this function really returns early
foldr' :
  (Bytes -> Maybe (CodePoint, Nat))  -- The peek function
  -> (Nat -> CodePoint -> Lazy a -> a)  -- The folding function, first arg = codepoint bytes
  -> Lazy a            -- The seed value
  -> Bytes   -- The bytes
  -> a
foldr' pE f z bs with (pE bs)
  | Nothing = z
  | Just (c, n) = f (S n) c (Delay $ foldr' pE f z (assert_smaller bs $ dropPrefix (cast $ S n) bs))

foldr : {e : Encoding} -> (CodePoint -> Lazy a -> a) -> a -> EncodedString e -> a
foldr {e = Enc pE _} f z (EncS bs) = foldr' pE (const f) z bs

foldl : {e : Encoding} -> (a -> CodePoint -> a) -> a -> EncodedString e -> a
foldl {e = Enc pE _} f z (EncS bs) = foldl' pE (const f) z bs

||| Turn a strict function into a lazy one for use with foldr.
mkLazy : (a -> b -> b) -> a -> Lazy b -> b
mkLazy f x (Delay xs) = f x xs

||| Unpack a string to the list of code points.
|||
||| Will overflow stack on long texts. Use reverse . foldl to avoid that.
unpack : {e : Encoding} -> EncodedString e -> List CodePoint
unpack {e = Enc pE _} = Data.Text.foldr (mkLazy (::)) List.Nil

||| O(n). Pack a list of code points into a string.
pack : {e : Encoding} -> List CodePoint -> EncodedString e
pack {e = Enc _ eE} = EncS . foldl (\bs, cp => bs ++ eE cp) empty

||| O(n). Convert a (possibly wide-char) String to Text.
||| Intended for Text literals.
str : String -> Text
str = pack . map fromChar . unpack

||| O(1). Construct a single-char encoded string.
singleton : {e : Encoding} -> CodePoint -> EncodedString e
singleton {e = Enc _ eE} c = EncS (eE c)

||| O(1). Construct an empty encoded string.
empty : EncodedString e
empty = EncS empty

||| O(n)! Prepend a single character. Consider using `snoc` instead.
cons : {e : Encoding} -> CodePoint -> EncodedString e -> EncodedString e
cons {e = Enc pE eE} c (EncS bs) = EncS (eE c ++ bs)

||| O(1). Uncons the first character or return Nothing if the string is empty.
uncons : {e : Encoding} -> EncodedString e -> Maybe (CodePoint, EncodedString e)
uncons {e = Enc pE eE} (EncS bs) with (pE bs)
  | Just (c, skip) = Just (c, EncS $ dropPrefix (cast $ S skip) bs)
  | Nothing        = Nothing

||| O(1) amortized. Append a single character.
snoc : {e : Encoding} -> EncodedString e -> CodePoint -> EncodedString e
snoc {e = Enc pE eE} (EncS bs) c = EncS (bs ++ eE c)

||| O(1). Get the first character or Nothing if the string is empty.
head : {e : Encoding} -> EncodedString e -> Maybe CodePoint
head {e = Enc pE eE} = map fst . pE . getBytes

||| O(1). Get the tail of the string or Nothing if the string is empty.
tail : {e : Encoding} -> EncodedString e -> Maybe (EncodedString e)
tail {e = Enc pE eE} = map snd . uncons

||| O(n). Get the length of the string. (Count all codepoints.)
length : EncodedString e -> Nat
length = Text.foldl (\n, _ => S n) Z

||| O(n_right). Concatenate two strings.
append : EncodedString e -> EncodedString e -> EncodedString e
append (EncS s) (EncS s') = EncS (s <+> s')

-- There is no (++) because it's got the wrong associativity.
-- Use (<+>) instead, which is left-associative.

implementation Semigroup (EncodedString e) where
  (<+>) = append

implementation Monoid (EncodedString e) where
  neutral = empty

-- init is not implemented yet
-- last is not implemented yet

||| O(1). Determines whether the string is empty.
null : EncodedString e -> Bool
null (EncS bs) = length bs == 0

||| O(n). Transform each code point in the string.
map : (CodePoint -> CodePoint) -> EncodedString e -> EncodedString e'
map {e' = Enc _ eE'} f = Text.foldl (\bs, c => bs `snoc` f c) empty

implementation Cast (EncodedString e) (EncodedString e') where
  cast = map id

||| O(n). Concatenate with separators.
intercalate : EncodedString e -> List (EncodedString e) -> EncodedString e
intercalate sep []        = empty
intercalate sep (x :: xs) = foldl (\acc, y => acc <+> sep <+> y) x xs

-- TODO: is it correct to do this in Unicode?
||| O(n). Rearrange the codepoints in the opposite order.
reverse : EncodedString e -> EncodedString e
reverse = Text.foldr (\x, xs => xs `snoc` x) empty

||| O(n). Concatenate all strings in the list.
concat : List (EncodedString e) -> EncodedString e
concat = foldl append empty

||| O(n). Concatenate all strings resulting from a codepoint mapping.
concatMap : (CodePoint -> EncodedString e) -> EncodedString e -> EncodedString e
concatMap f = foldl (\bs, cp => bs <+> f cp) empty

||| O(n). Check whether any codepoint has the specified property.
|||
||| Will overflow the stack on very long texts
||| but will exit early if the desired codepoint is found.
any : (CodePoint -> Bool) -> EncodedString e -> Bool
any p = foldr ((||) . p) False

||| O(n). Check whether all codepoints have the specified property.
all : (CodePoint -> Bool) -> EncodedString e -> Bool
all p = foldl (\r => \c => r && p c) True

||| O(n*|w|). Repeat the string n times.
replicate : Nat -> EncodedString e -> EncodedString e
replicate    Z  s = empty
replicate (S n) s = replicate n s <+> s

spanByteLength : {e : Encoding} -> (CodePoint -> Bool) -> EncodedString e -> Nat
spanByteLength {e = Enc pE _} p (EncS bs) = foldr' pE f Z bs
  where
    f : Nat -> CodePoint -> (rest : Lazy Nat) -> Nat
    f nbytes cp rest = if p cp then nbytes + rest else 0

takeBytes : Nat -> EncodedString e -> EncodedString e
takeBytes n (EncS bs) = EncS (takePrefix (cast n) bs)

dropBytes : Nat -> EncodedString e -> EncodedString e
dropBytes n (EncS bs) = EncS (dropPrefix (cast n) bs)

||| O(n). Return the longest prefix of code points that satisfy the predicate.
takeWhile : (CodePoint -> Bool) -> EncodedString e -> EncodedString e
takeWhile p s = spanByteLength p s `takeBytes` s

||| O(n). Drop the longest prefix of code points that satisfy the predicate.
dropWhile : (CodePoint -> Bool) -> EncodedString e -> EncodedString e
dropWhile p s = spanByteLength p s `dropBytes` s

||| O(n). Slightly more efficient equivalent to (takeWhile p, dropWhile p)
span : (CodePoint -> Bool) -> EncodedString e -> (EncodedString e, EncodedString e)
span p s = let n = spanByteLength p s in (takeBytes n s, dropBytes n s)

||| O(n). Defined as span (not . p).
break : (CodePoint -> Bool) -> EncodedString e -> (EncodedString e, EncodedString e)
break p = span (not . p)

cpointsToBytes : (pE : Bytes -> Maybe (CodePoint, Nat)) -> Nat -> Nat -> Bytes -> Nat
cpointsToBytes pE    Z  k bs = k
cpointsToBytes pE (S n) k bs with (pE bs)
  | Nothing = k
  | Just (c, nbytes) = cpointsToBytes pE n (S nbytes + k) (dropPrefix (cast $ S nbytes) bs)

||| O(n). Extract the first N code points.
take : {e : Encoding} -> Nat -> EncodedString e -> EncodedString e
take {e = Enc pE _} n (EncS bs) = EncS (takePrefix (cast $ cpointsToBytes pE n 0 bs) bs)

||| O(n). Drop the first N code points.
drop : {e : Encoding} -> Nat -> EncodedString e -> EncodedString e
drop {e = Enc pE _} n (EncS bs) = EncS (dropPrefix (cast $ cpointsToBytes pE n 0 bs) bs)

||| O(n). Split after the N-th code point.
splitAt : {e : Encoding} -> Nat -> EncodedString e -> (EncodedString e, EncodedString e)
splitAt {e = Enc pE _} n (EncS bs) = (EncS $ takePrefix nbytes bs, EncS $ dropPrefix nbytes bs)
  where
    nbytes = cast $ cpointsToBytes pE n 0 bs

||| O(n). (foldr-)Traverse all spans of code points satisfying the predicate.
foldSpans : (CodePoint -> Bool) -> (EncodedString e -> Lazy a -> a) -> a -> EncodedString e -> a
foldSpans p f z s with (null s)
  | True  = z
  | False =
      let (c, rest) = span p s in
        f c (Delay $ foldSpans p f z (assert_smaller s $ drop 1 rest))

||| O(n). Count all spans of code points satisfying the predicate.
spanCount : (CodePoint -> Bool) -> EncodedString e -> Nat
spanCount p = foldSpans p (mkLazy $ const S) Z

||| O(n). Split into spans separated by code points satisfying the predicate, discarding the separators.
split : (sep : CodePoint -> Bool) -> EncodedString e -> List (EncodedString e)
split sep = foldSpans (not . sep) (mkLazy (::)) []

||| O(n). Split to lines, according to Data.Text.CodePoint.isNewline.
lines : EncodedString e -> List (EncodedString e)
lines = split isNewline

||| O(n). Count lines, according to Data.Text.CodePoint.isNewline.
lineCount : EncodedString e -> Nat
lineCount = spanCount (not . isNewline)

||| O(n). Split to words, according to Data.Text.CodePoint.isSpace.
words : EncodedString e -> List (EncodedString e)
words = split isSpace

||| O(n). Count words, according to Data.Text.CodePoint.isSpace.
wordCount : EncodedString e -> Nat
wordCount = spanCount (not . isSpace)

||| O(n). Remove whitespace from both ends of the string.
trim : EncodedString e -> EncodedString e
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

toUTF8 : EncodedString e -> EncodedString UTF8.UTF8
toUTF8 {e = UTF8.UTF8} = id
-- toUTF8 {e = e}         = pack . unpack  -- ??? unreachable case? wth?

implementation Show (EncodedString e) where
  show = toString . getBytes . toUTF8
