module Main

import Data.Bits
import Data.Text
import Data.ByteString
import Data.Text.CodePoint

decodesTo : String -> List Int -> IO ()
decodesTo s expected = putStrLn $ if decoded == expected
    then "PASS"
    else "FAIL: \n"
      ++ "  chars   : " ++ show chars ++ "\n"
      ++ "  bytes   : " ++ show bytes ++ "\n"
      ++ "  expected: " ++ show expected ++ "\n"
      ++ "  decoded : " ++ show decoded
  where
    chars : List Char
    chars = unpack s

    bytes : List Int
    bytes = map (flip mod 256 . (+256) . Prelude.Char.ord) chars

    decoded : List Int
    decoded = map Data.Text.CodePoint.ord . unpack . fromUTF8 . fromString $ s

encDec : String -> IO ()
encDec input = do
    print input
    putStrLn $ if encoded == inputBS
      then "  PASS[enc]"
      else "  FAIL[enc]: " ++ show encoded ++ " != " ++ show inputBS ++ "\n"
    putStrLn $ if decoded == inputText
      then "  PASS[dec]"
      else "  FAIL[dec]: " ++ show decoded ++ " != " ++ show inputText ++ "\n"
  where
    inputBS : ByteString
    inputBS = fromString input

    encoded : ByteString
    encoded = getBytes (str input)

    decoded : Text
    decoded = fromUTF8 encoded

    inputText : Text
    inputText = str input
    
main : IO ()
main = sequence_
    -- ASCII compatibility
    [ "Hello World!" `decodesTo` (map ord' $ unpack "Hello World!")

    -- Continuation byte without a character having started is recognised
    , "\x80" `decodesTo` [repChar]

    -- Overlong zero is not accepted
    , "\xC0\x80" `decodesTo` [repChar]

    -- Overlong double quote is not accepted
    , "\xC0\xA2" `decodesTo` [repChar]

    -- Garbage is skipped correctly
    , "-\x80\x80\x80-*-" `decodesTo` [ord' '-', repChar, ord' '-', ord' '*', ord' '-']

    -- Agda compatibility: "λα → « φ ∘ κ » ∷ α"
    -- Idris parses string literals as Unicode but we need an array of bytes instead.
    , "\xce\xbb\xce\xb1 \xe2\x86\x92 \xc2\xab \xcf\x86 \xe2\x88\x98 \xce\xba \xc2\xbb \xe2\x88\xb7 \xce\xb1"
        `decodesTo` [955,945,32,8594,32,171,32,966,32,8728,32,954,32,187,32,8759,32,945]

    -- Truncated characters are handled correctly.
    , "x\xe2\x88" `decodesTo` [ord' 'x', repChar]

    , encDec "Hello world!"
    , encDec ""
    , encDec "123546"
    , encDec "\x10\x20\x30\x40"
    ]
  where
    ord' : Char -> Int
    ord' = ord

    repChar : Int
    repChar = 0xFFFD

