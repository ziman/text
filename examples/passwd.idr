module Main

import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding
import Data.Text.Encoding.UTF8
import Data.Text.CodePoint

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Text

record Entry where
  constructor E
  user : Text
  uid : Integer
  gid : Integer
  name : Text
  homedir : FileName
  shell : FileName

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
  = TIO.putStrLn $ str "user " <+> user <+> str " has the homedir " <+> homedir

main : IO ()
main = do
  Right passwd <- readTextFile (str "input.txt") UTF8  -- Encoding always explicit
    | Left err => printLn err
  case parse pEntries passwd of
    Right es  => traverse_ printEntry es
    Left  err => TIO.putStrLn err
