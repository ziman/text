module Main

main : IO ()
main = do
  Right stuff <- readFile "input.txt"
    | Left err => printLn err
  printLn . List.length . lines $ stuff
