module Lightyear.Text

import Control.Monad.Identity

import Data.Text
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
  lineLengths = map (nat2int . Data.Text.length) . Data.Text.lines

ParserE : Encoding -> Type -> Type
ParserE e = ParserT Identity (EncodedString e)

Parser : Type -> Type
Parser = ParserE UTF8

parse : ParserE e a -> (EncodedString e) -> Either Text a
parse f s = let Id r = execParserT f s in case r of
  Success _ x => Right x
  Failure es => Left . fromUTF8 . fromString $ formatError s es -- for now

satisfy : Monad m => (CodePoint -> Bool) -> ParserT m (EncodedString e) CodePoint
satisfy = satisfy' (St Data.Text.uncons)

satisfyMaybe : Monad m => (CodePoint -> Maybe out) -> ParserT m (EncodedString e) out
satisfyMaybe = satisfyMaybe' (St Data.Text.uncons)

codepoint : Monad m => CodePoint -> ParserT m (EncodedString e) ()
codepoint c = skip (satisfy (== c)) <?> "codepoint '" ++ show c ++ "'"

asciiChar : Monad m => Char -> ParserT m (EncodedString e) ()
asciiChar c = codepoint (fromChar c) <?> "ASCII character '" ++ singleton c ++ "'"

ascii : Monad m => String -> ParserT m (EncodedString e) ()
ascii s = traverse_ asciiChar (unpack s) <?> "ASCII string " ++ show s

-- TODO: prefix check can be done much more efficiently than characterwise
--       (if the encodings are the same)
-- TODO: change error messages to support Text
text : Monad m => EncodedString e -> ParserT m (EncodedString e) ()
text s = traverse_ codepoint (Data.Text.unpack s) <?> "text " ++ (toString . getBytes $ s)

-- TODO: use the Unicode definition of isSpace
space : Monad m => ParserT m (EncodedString e) ()
space = skip (many $ satisfy isSpace) <?> "whitespace"

token : Monad m => Text -> ParserT m (EncodedString e) ()
token s = skip (text s) <$ space <?> "token " ++ show s

parens : Monad m => ParserT m (EncodedString e) a -> ParserT m (EncodedString e) a
parens p = asciiChar '(' $> p <$ asciiChar ')'

digit : Monad m => ParserT m (EncodedString e) (Fin 10)
digit = satisfyMaybe (fromChar . chr8)
  where fromChar : Char -> Maybe (Fin 10)
        fromChar '0' = Just fZ
        fromChar '1' = Just (fS (fZ))
        fromChar '2' = Just (fS (fS (fZ)))
        fromChar '3' = Just (fS (fS (fS (fZ))))
        fromChar '4' = Just (fS (fS (fS (fS (fZ)))))
        fromChar '5' = Just (fS (fS (fS (fS (fS (fZ))))))
        fromChar '6' = Just (fS (fS (fS (fS (fS (fS (fZ)))))))
        fromChar '7' = Just (fS (fS (fS (fS (fS (fS (fS (fZ))))))))
        fromChar '8' = Just (fS (fS (fS (fS (fS (fS (fS (fS (fZ)))))))))
        fromChar '9' = Just (fS (fS (fS (fS (fS (fS (fS (fS (fS (fZ))))))))))
        fromChar _ = Nothing

integer : (Num n, Monad m) => ParserT m (EncodedString e) n
integer = do minus <- opt (asciiChar '-')
             ds <- some digit
             let theInt = getInteger ds
             case minus of
               Nothing => pure (fromInteger theInt)
               Just () => pure (fromInteger ((-1) * theInt))
  where getInteger : List (Fin 10) -> Integer
        getInteger = foldl (\a => \b => 10 * a + cast b) 0

test : ParserE e a -> EncodedString e -> IO (Maybe a)
test p s = case parse p s of
  Left e => putStrLn (getBytes e) $> pure Nothing
  Right x => pure (Just x)
