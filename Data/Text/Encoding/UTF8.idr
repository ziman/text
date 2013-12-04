module Data.Text.Encoding.UTF8

import Data.Text.Encoding

%access private

unconsU : ByteString -> Maybe (CodePoint, ByteString)
unconsU bs = ?uncons

consU : CodePoint -> ByteString -> ByteString
consU c bs = ?consU

public
UTF8 : Encoding
UTF8 = Enc unconsU consU
