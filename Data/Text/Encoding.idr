module Data.Text.Encoding

import Data.Bits
import Data.Bytes
import Data.Text.CodePoint

%access public
%default total

||| The type of text encodings.
record Encoding where
  constructor Enc

  ||| Return the first code point and the predecessor of
  ||| the number of bytes to skip to get past the first code point.
  |||
  ||| In other words, returning (c, n)
  ||| will make the decoder skip (S n) bytes.
  peek : Bytes -> Maybe (CodePoint, Nat)

  ||| Convert a code point to bytes.
  encode : CodePoint -> Bytes
