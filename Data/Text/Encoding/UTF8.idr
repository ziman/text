module Data.Text.Encoding.UTF8

import Data.Bits
import Data.Text.Encoding

%access private

-- As defined by Unicode.
replacementChar : CodePoint
replacementChar = 0xFFFD

countTrash : ByteString -> Nat
countTrash = Prelude.Strings.length . fst . span (isCont . ord)
  where
    isCont : Int -> Bool
    isCont c = (c >= 0x80) && (c < 0xC0)

decode2 : Int -> ByteString -> CodePoint
decode2 p bs = ?decode2'

decode3 : Int -> ByteString -> CodePoint
decode3 p bs = ?decode3'

decode4 : Int -> ByteString -> CodePoint
decode4 p bs = ?decode4'

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
                else Just (replacementChar, countTrash xs)

consU : CodePoint -> ByteString
consU c = ?consU'

public
UTF8 : Encoding
UTF8 = Enc unconsU consU
