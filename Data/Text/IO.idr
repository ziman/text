module Data.Text.IO

import Data.ByteString
import Data.Text
import Data.Text.Encoding

%access public
%default partial

total
FileName : Type
FileName = Text

abstract
record TextFile : Encoding -> Type where
  TextF : (handle_ : File) -> TextFile e

infix 3 <@>
private
(<@>) : (a -> b) -> IO a -> IO b
(<@>) = map

getLine : {e : Encoding} -> IO (EncodedString e)
getLine {e = e} = (\s => fromString s `asEncodedIn` e) <@> Prelude.getLine

putStr : EncodedString e -> IO ()
putStr = Prelude.putStr . toString . getBytes

putStrLn : EncodedString e -> IO ()
putStrLn = Prelude.putStrLn . toString . getBytes

-- We want the users to state the encoding explicitly.
openTextFile : FileName -> (e : Encoding) -> Mode -> IO (TextFile e)
openTextFile name e mode = TextF <@> Prelude.openFile nameS mode
  where
    nameS = toString . getBytes $ name

closeFile : TextFile e -> IO ()
closeFile (TextF h) = Prelude.closeFile h

fread : TextFile e -> IO (EncodedString e)
fread {e = e} (TextF h) = (\s => fromString s `asEncodedIn` e) <@> Prelude.fread h

fwrite : TextFile e -> EncodedString e -> IO ()
fwrite (TextF h) = Prelude.fwrite h . toString . getBytes

feof : TextFile e -> IO Bool
feof (TextF h) = Prelude.feof h

ferror : TextFile e -> IO Bool
ferror (TextF h) = Prelude.ferror h

validFile : TextFile e -> IO Bool
validFile (TextF h) = Prelude.validFile h

-- O(n^2), no error checking. Use with caution.
partial
readTextFile : FileName -> (e : Encoding) -> IO (EncodedString e)
readTextFile fn e = do
    f <- openTextFile fn e Read
    c <- readFile' f empty
    closeFile f
    return c
  where
    partial
    readFile' : TextFile e -> EncodedString e -> IO (EncodedString e)
    readFile' f s = do
      if !(feof f)
        then return s
        else fread f >>= readFile' f . (s ++)

partial
writeTextFile : FileName -> (e : Encoding) -> EncodedString e -> IO ()
writeTextFile fn e s = do
  f <- openTextFile fn e Write
  fwrite f s
  closeFile f
