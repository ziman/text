module main

import Data.Bits
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

decodesTo : String -> List Int -> Type
decodesTo s cs = so (unpack (s `asEncodedIn` UTF8) == map (intToBits . cast) cs)

replacementChar : Int
replacementChar = 0xFFFD

-- ASCII compatibility
test1 : "Hello World!" `decodesTo` (map ord $ unpack "Hello World!")
test1 = oh

-- Continuation byte without a character having started is recognised
test2 : "\x80" `decodesTo` [replacementChar]
test2 = oh

-- Overlong zero is not accepted
test3 : "\xC0\x80" `decodesTo` [replacementChar]
test3 = oh

-- Overlong double quote is not accepted
test4 : "\xC0\xA2" `decodesTo` [replacementChar]
test4 = oh

-- Garbage is skipped correctly
test5 : "-\x80\x80\x80-*-" `decodesTo` [ord '-', replacementChar, ord '-', ord '*', ord '-']
test5 = oh

{-
-- Agda compatibility
test6 : "λα → « φ ∘ κ » ∷ α" `decodesTo` [955,945,32,8594,32,171,32,966,32,8728,32,954,32,187,32,8759,32,945]
test6 = oh
-}

main : IO ()
main = putStrLn "OK"
