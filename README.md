# Text #

This library is an attempt to clean up text processing in Idris,
especially with respect to text encodings.

The idea is that a string is a sequence of Unicode code points; this
sequence can be encoded in various ways: a sequence of bytes
(like UTF-8-encoded strings), a `List` of code points (like `String`
in Haskell), etc.

This library should provide efficient conversion between the
representations, along with a convenient interface to them.

**MORE DOC TBD**
