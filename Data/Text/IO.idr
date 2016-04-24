module Data.Text.IO

import Data.Bytes
import Data.Text
import Data.Text.Encoding

%access public export
%default partial

||| A FileName is a piece of Text.
total
FileName : Type
FileName = Text

||| Wrapper for a file handle, indexed by a text encoding.
export
record TextFile (e : Encoding) where
  constructor TextF
  handle : File

getLine : {e : Encoding} -> IO (EncodedString e)
getLine {e = e} = (\s => fromString s `asEncodedIn` e) <$> Interactive.getLine

putStr : EncodedString e -> IO ()
putStr = Interactive.putStr . toString . getBytes

putStrLn : EncodedString e -> IO ()
putStrLn = Interactive.putStrLn . toString . getBytes

||| Open a text file.
||| We want the users to state the encoding explicitly here.
openTextFile : FileName -> (e : Encoding) -> Mode -> IO (Either FileError (TextFile e))
openTextFile name e mode = map TextF <$> Prelude.File.openFile nameS mode
  where
    nameS = toString . getBytes $ name

closeFile : TextFile e -> IO ()
closeFile (TextF h) = Prelude.File.closeFile h

fGetLine : TextFile e -> IO (Either FileError (EncodedString e))
fGetLine {e = e} (TextF h) = map (\s => fromString s `asEncodedIn` e) <$> Prelude.File.fGetLine h

fPutStr : TextFile e -> EncodedString e -> IO (Either FileError ())
fPutStr (TextF h) = Prelude.File.fPutStr h . toString . getBytes

fEOF : TextFile e -> IO Bool
fEOF (TextF h) = Prelude.File.fEOF h

ferror : TextFile e -> IO Bool
ferror (TextF h) = Prelude.File.ferror h

validFile : TextFile e -> IO Bool
validFile (TextF h) = Prelude.File.validFile h

-- The following code is ugly and hacky

||| Read a text file into an encoded string.
||| O(n), no error checking. Use with caution.
partial
readTextFile : FileName -> (e : Encoding) -> IO (Either FileError (EncodedString e))
readTextFile fn e = do
    Right f <- openTextFile fn e Read
      | Left e => return (Left e)
    result <- readFile' f (Right empty)
    closeFile f
    return result
  where
    partial
    readFile' : TextFile e -> (Either FileError (EncodedString e)) -> IO (Either FileError (EncodedString e))
    readFile' f (Left  e) = return $ Left e
    readFile' f (Right s) = do
      if !(fEOF f)
        then return (Right s)
        else fGetLine f >>= readFile' f . (map (s <+>))

partial
writeTextFile : FileName -> (e : Encoding) -> EncodedString e -> IO (Either FileError ())
writeTextFile fn e s = do
  Right f <- openTextFile fn e WriteTruncate
    | Left e => return (Left e)
  Right () <- fPutStr f s
    | Left e => return (Left e)
  Right <$> closeFile f
