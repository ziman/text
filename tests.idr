module main

import Data.Bits
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

decodesTo : String -> List Int -> Type
decodesTo s cs = so (unpack (s `asEncodedIn` UTF8) == map (intToBits . cast) cs)

-- test1 : "HELLO" `decodesTo` [ord 'H', ord 'E', ord 'L', ord 'L', ord 'O']
-- test1 = oh

main : IO ()
main = putStrLn "OK"
