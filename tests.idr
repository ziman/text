module main

import Data.Bits
import Data.Text
import Data.Text.Encoding

test : String -> List Int -> Type
test s cs = so (unpack (the Text (cast s)) == map (intToBits . cast) cs)

-- test1 : test "HELLO" [ord 'H', ord 'E', ord 'L', ord 'L', ord 'O']
-- test1 = oh

main : IO ()
main = putStrLn "OK"
