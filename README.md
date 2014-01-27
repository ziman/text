# Text #

This library is an attempt to clean up text processing in Idris,
especially with respect to text encodings.

The idea is that a string is a sequence of Unicode scalar values; this
sequence can be encoded in various ways
* a sequence of bytes
  * UTF-8
  * UTF-16
  * etc.
* a `List` of code points
  * like `String` in Haskell
* etc.

This library should provide a convenient interface to them.

**MORE DOC TBD**

## Key types

* `Data.Text.Text` as a specialisation of `Data.Text.EncodedString`
* `Data.Text.CodePoint.CodePoint`
* `Data.ByteString.ByteString`
* `Data.Text.Encoding.Encoding`

## Note

This is mostly an API prototype and its implementation
should certainly be improved in various ways, especially
wrt. performance and error checking.

(A very non-exhaustive) list of things to do:

* Create `Text`-only specialisations of the current modules (`Data.Text`,
  `Lightyear.Text`) that define `EncodedString e` to aid elaboration.
  (The encoding `e` is sometimes not possible to infer if it is left
  totally general.)

* Use a more efficient `ByteString` back-end instead of the current one,
  which abuses `String`.

## Related work

* https://github.com/idris-lang/Idris-dev/issues/802
* https://docs.google.com/document/d/1MTvYOXuoumTw1N32pXRm0k8MtcM04PZQIceTO7vhO-k/edit#heading=h.minhdz3c63y7
