module Main

import Data.Text as T
import Data.Text.IO as TIO

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Text

record Entry : Type where
  E :
    (user : Text)
    -> (uid, gid : Int)
    -> (name : Text)
    -> (homedir : FileName)
    -> (shell : FileName)
    -> Entry

-- We use fromChar to use Char literals as CodePoints.
field : Parser Text
field = T.pack `map` many (satisfy isFieldChar)
  where
    isFieldChar x =
      x /= fromChar ':'
      && x /= fromChar '\n'

filename : Parser FileName
filename = field

pEntry : Parser Entry
pEntry = do
  user <- field
  ascii ":x:"    -- use a String literal to parse verbatim Text
  uid <- integer
  char ':'       -- use a Char literal to parse the corresponding CodePoint
  gid <- integer
  char ':'
  name <- field
  char ':'
  homedir <- filename
  char ':'
  shell <- filename
  space
  return $ E user uid gid name homedir shell

pEntries : Parser (List Entry)
pEntries = many pEntry

-- Here we use the encoding-aware putStrLn from Data.Text.IO.
-- Also, we use the function "str" for Text literals.
printEntry : Entry -> IO ()
printEntry (E user uid gid name homedir shell)
  = TIO.putStrLn $ str "user " ++ user ++ str " has the homedir " ++ homedir

main : IO ()
main = do
  passwd <- readTextFile (str "/etc/passwd") UTF8  -- Encoding always explicit
  case parse pEntries passwd of
    Right es  => traverse_ printEntry es
    Left  err => TIO.putStrLn err
