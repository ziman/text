module Data.Text.Encoding

import Data.Bits
import Data.ByteString
import Data.Text.CodePoint

%access public
%default total

||| The type of text encodings.
record Encoding : Type where
  ||| The encoding constructor.
  ||| @ peek_ returns the first code point
  |||         and the predecessor of the number of bytes
  |||         to skip to get past the first code point.
  |||
  |||         In other words, returning (c, n)
  |||         will make the decoder skip (S n) bytes.
  |||
  ||| @encode_ converts a codepoint to bytes.
  Enc :
       (peek_   : ByteString -> Maybe (CodePoint, Nat))
    -> (encode_ : CodePoint -> ByteString)
    -> Encoding
