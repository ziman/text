module Main

import Data.Text
import Data.Text.IO
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

main : IO ()
main = do
  Right stuff <- readTextFile (str "input.txt") UTF8
    | Left err => printLn err
  printLn . List.length . lines $ stuff
