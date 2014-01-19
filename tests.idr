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

-- Continuation byte without a character having started
test2 : "\x80" `decodesTo` [replacementChar]
test2 = oh

-- Overlong zero is not accepted
test3 : "\xC0\x80" `decodesTo` [replacementChar]
test3 = oh

main : IO ()
main = putStrLn "OK"
