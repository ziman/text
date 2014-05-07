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

field : Parser Text
field = T.pack `map` many (satisfy (\x => x /= fromChar ':' && x /= fromChar '\n'))

filename : Parser FileName
filename = field

pEntry : Parser Entry
pEntry = do
  user <- field
  ascii ":x:"
  uid <- integer
  char ':'
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

printEntry : Entry -> IO ()
printEntry (E user uid gid name homedir shell)
  = TIO.putStrLn $ str "user " ++ user ++ str " has the homedir " ++ homedir

main : IO ()
main = do
  passwd <- readTextFile (str "/etc/passwd") UTF8
  case parse pEntries passwd of
    Right es  => traverse_ printEntry es
    Left  err => TIO.putStrLn err
