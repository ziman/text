module Data.Text.Encoding.UTF8

import Data.Bits
import Data.Text.Encoding

%access private

-- As defined by Unicode.
replacementChar : CodePoint
replacementChar = 0xFFFD

countTrash : ByteString -> Nat
countTrash bs = ?countTrash'

decode2 : Int -> ByteString -> CodePoint
decode2 p bs = ?decode2'

decode3 : Int -> ByteString -> CodePoint
decode3 p bs = ?decode3'

decode4 : Int -> ByteString -> CodePoint
decode4 p bs = ?decode4'

decode5 : Int -> ByteString -> CodePoint
decode5 p bs = ?decode5'

decode6 : Int -> ByteString -> CodePoint
decode6 p bs = ?decode6'

unconsU : ByteString -> Maybe (CodePoint, Nat)
unconsU bs with (strM bs)
  unconsU ""             | StrNil       = Nothing
  unconsU (strCons x xs) | StrCons x xs =
    let x' = ord x in
      if x' < 0x80
        then Just (x', 0)
        else if x' < 0xC0
          then Just (replacementChar, countTrash xs)
          else if x' < 0xE0
            then Just (decode2 x' xs, 1)
            else if x' < 0xF0
              then Just (decode3 x' xs, 2)
              else if x' < 0xF8
                then Just (decode4 x' xs, 3)
                else if x' < 0xFC
                  then Just (decode5 x' xs, 4)
                  else Just (decode6 x' xs, 5)

consU : CodePoint -> ByteString
consU c = ?consU'

public
UTF8 : Encoding
UTF8 = Enc unconsU consU
