module Lightyear.Text

import Control.Monad.Identity

import Data.Fin

import Data.Text as T
import Data.Text.IO
import Data.Text.CodePoint
import Data.Text.Encoding
import Data.Text.Encoding.UTF8

import Lightyear.Core
import Lightyear.Errmsg
import Lightyear.Combinators

%access public

private
nat2int : Nat -> Int
nat2int Z = 0
nat2int (S x) = 1 + nat2int x

implementation Layout (EncodedString e) where
  lineLengths = map (nat2int . T.length) . T.lines

implementation Stream CodePoint (EncodedString e) where
  uncons = T.uncons

total
ParserE : Encoding -> Type -> Type
ParserE e = ParserT Identity (EncodedString e)

total
Parser : Type -> Type
Parser = ParserE UTF8

parse : ParserE e a -> (EncodedString e) -> Either Text a
parse f s = let Id r = execParserT f s in case r of
  Success _ x => Right x
  Failure es => Left . str $ formatError s es -- for now

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
text s = traverse_ codepoint (T.unpack s) <?> "text " ++ show s

-- TODO: use the Unicode definition of isSpace
space : Monad m => ParserT m (EncodedString e) ()
space = skip $ many spaces
  where
    -- lifted because of class resolution problems
    spaces : Monad m => ParserT m (EncodedString e) CodePoint
    spaces = satisfy isSpace

token : Monad m => Text -> ParserT m (EncodedString e) ()
token s = skip (text s) <* space <?> "token " ++ show s

parens : Monad m => ParserT m (EncodedString e) a -> ParserT m (EncodedString e) a
parens p = char '(' *> p <* char ')'

digit : Monad m => ParserT m (EncodedString e) (Fin 10)
digit = satisfyMaybe (fromChar . CodePoint.ord)
  where fromChar : Int -> Maybe (Fin 10)
        fromChar 0x30 = Just 0
        fromChar 0x31 = Just 1
        fromChar 0x32 = Just 2
        fromChar 0x33 = Just 3
        fromChar 0x34 = Just 4
        fromChar 0x35 = Just 5
        fromChar 0x36 = Just 6
        fromChar 0x37 = Just 7
        fromChar 0x38 = Just 8
        fromChar 0x39 = Just 9
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
  Left e => putStrLn e *> pure Nothing
  Right x => pure (Just x)
