module Data.Text.Encoding.UTF8

import Data.Text.Encoding

%access private

unconsU : ByteString -> Maybe (CodePoint, Nat)
unconsU bs = ?unconsU

consU : CodePoint -> ByteString
consU c = ?consU

public
UTF8 : Encoding
UTF8 = Enc unconsU consU
