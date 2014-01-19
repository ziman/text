module Main

import Data.Bits
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

decodesTo : String -> List Int -> IO ()
decodesTo s cs = putStrLn $ if decoded == expected
    then "PASS"
    else "FAIL: \n"
      ++ "  chars   : " ++ show chars ++ "\n"
      ++ "  bytes   : " ++ show bytes ++ "\n"
      ++ "  expected: " ++ show expected ++ "\n"
      ++ "  decoded : " ++ show decoded
  where
    chars : List Char
    chars = unpack s

    bytes : List Integer
    bytes = map (flip mod 256 . (+256) . cast . ord) chars

    decoded : List Integer
    decoded = map bitsToInt . unpack $ (s `asEncodedIn` UTF8)
    
    expected : List Integer
    expected = map cast cs

main : IO ()
main = sequence_
    -- ASCII compatibility
    [ "Hello World!" `decodesTo` (map ord $ unpack "Hello World!")

    -- Continuation byte without a character having started is recognised
    , "\x80" `decodesTo` [replacementChar]

    -- Overlong zero is not accepted
    , "\xC0\x80" `decodesTo` [replacementChar]

    -- Overlong double quote is not accepted
    , "\xC0\xA2" `decodesTo` [replacementChar]

    -- Garbage is skipped correctly
    , "-\x80\x80\x80-*-" `decodesTo` [ord '-', replacementChar, ord '-', ord '*', ord '-']

    -- Agda compatibility: "λα → « φ ∘ κ » ∷ α"
    -- Idris parses string literals as Unicode but we need an array of bytes instead.
    , "\xce\xbb\xce\xb1 \xe2\x86\x92 \xc2\xab \xcf\x86 \xe2\x88\x98 \xce\xba \xc2\xbb \xe2\x88\xb7 \xce\xb1"
        `decodesTo` [955,945,32,8594,32,171,32,966,32,8728,32,954,32,187,32,8759,32,945]

    -- Truncated characters are handled correctly.
    , "x\xe2\x88" `decodesTo` [ord 'x', replacementChar]
    ]
  where
    replacementChar : Int
    replacementChar = 0xFFFD

