module Lightyear.Text

import Control.Monad.Identity

import Data.Text as T
import Data.Text.IO
import Data.Text.CodePoint
import Data.Text.Encoding.UTF8

import Lightyear.Core
import Lightyear.Errmsg
import Lightyear.Combinators

%access public

private
nat2int : Nat -> Int
nat2int Z = 0
nat2int (S x) = 1 + nat2int x

instance Layout (EncodedString e) where
  lineLengths = map (nat2int . T.length) . T.lines

total
ParserE : Encoding -> Type -> Type
ParserE e = ParserT Identity (EncodedString e)

total
Parser : Type -> Type
Parser = ParserE UTF8

parse : ParserE e a -> (EncodedString e) -> Either Text a
parse f s = let Id r = execParserT f s in case r of
  Success _ x => Right x
  Failure es => Left . fromUTF8 . fromString $ formatError s es -- for now

satisfy : Monad m => (CodePoint -> Bool) -> ParserT m (EncodedString e) CodePoint
satisfy = satisfy' (St T.uncons)

satisfyMaybe : Monad m => (CodePoint -> Maybe out) -> ParserT m (EncodedString e) out
satisfyMaybe = satisfyMaybe' (St T.uncons)

codepoint : Monad m => CodePoint -> ParserT m (EncodedString e) ()
codepoint c = skip (satisfy (== c)) <?> "codepoint '" ++ show c ++ "'"

char : Monad m => Char -> ParserT m (EncodedString e) ()
char c = codepoint (fromChar c) <?> "ASCII character '" ++ singleton c ++ "'"

ascii : Monad m => String -> ParserT m (EncodedString e) ()
ascii s = traverse_ char (unpack s) <?> "ASCII string " ++ show s

-- TODO: prefix check can be done much more efficiently than characterwise
--       (if the encodings are the same)
-- TODO: change error messages to support Text
text : Monad m => EncodedString e -> ParserT m (EncodedString e') ()
text s = traverse_ codepoint (T.unpack s) <?> "text " ++ (toString . getBytes $ s)

-- TODO: use the Unicode definition of isSpace
space : Monad m => ParserT m (EncodedString e) ()
space = skip (many $ satisfy isSpace) <?> "whitespace"

token : Monad m => Text -> ParserT m (EncodedString e) ()
token s = skip (text s) <$ space <?> "token " ++ show s

parens : Monad m => ParserT m (EncodedString e) a -> ParserT m (EncodedString e) a
parens p = char '(' $> p <$ char ')'

digit : Monad m => ParserT m (EncodedString e) (Fin 10)
digit = satisfyMaybe (fromChar . ord)
  where fromChar : Int -> Maybe (Fin 10)
        fromChar 0x30 = Just FZ
        fromChar 0x31 = Just (FS (FZ))
        fromChar 0x32 = Just (FS (FS (FZ)))
        fromChar 0x33 = Just (FS (FS (FS (FZ))))
        fromChar 0x34 = Just (FS (FS (FS (FS (FZ)))))
        fromChar 0x35 = Just (FS (FS (FS (FS (FS (FZ))))))
        fromChar 0x36 = Just (FS (FS (FS (FS (FS (FS (FZ)))))))
        fromChar 0x37 = Just (FS (FS (FS (FS (FS (FS (FS (FZ))))))))
        fromChar 0x38 = Just (FS (FS (FS (FS (FS (FS (FS (FS (FZ)))))))))
        fromChar 0x39 = Just (FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ))))))))))
        fromChar _ = Nothing

integer : (Num n, Monad m) => ParserT m (EncodedString e) n
integer = do minus <- opt (char '-')
             ds <- some digit
             let theInt = getInteger ds
             case minus of
               Nothing => pure (fromInteger theInt)
               Just () => pure (fromInteger ((-1) * theInt))
  where getInteger : List (Fin 10) -> Integer
        getInteger = foldl (\a => \b => 10 * a + cast b) 0

test : ParserE e a -> EncodedString e -> IO (Maybe a)
test p s = case parse p s of
  Left e => putStrLn e $> pure Nothing
  Right x => pure (Just x)
