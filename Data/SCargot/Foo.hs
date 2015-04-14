{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Foo where

import           Control.Applicative hiding ((<|>), many)
import           Data.Char
import           Data.Monoid ((<>))
import           Data.Text (Text, concatMap, pack, singleton)
import           Numeric (readDec, readFloat, readHex, readSigned)
import           Text.Parsec
import           Text.Parsec.Text
import           Text.Parsec.Token (float, integer, stringLiteral)
import           Text.Parsec.Language (haskell)

import           Prelude hiding (concatMap)

import Data.SCargot.Repr.Basic (SExpr)
import Data.SCargot.General



data Atom
  = AToken  Text
  | AString Text
  | AInt    Integer
  | AFloat  Double
    deriving (Eq, Show)

atomChar :: Parser Char
atomChar = satisfy go
  where go c = isAlphaNum c
          || c == '-' || c == '*' || c == '/'
          || c == '+' || c == '<' || c == '>'
          || c == '=' || c == '!' || c == '?'

pToken :: Parser Text
pToken = pack <$> ((:) <$> letter <*> many atomChar)

pString :: Parser Text
pString = pack <$> between (char '"') (char '"') (many (val <|> esc))
  where val = Just <$> satisfy (\ c -> c /= '"' && c /= '\\' && c > '\026')
        esc = do char '\\'
                 Nothing <$ (gap <|> char '&') <|>
                   Just <$> cod
        gap = many1 space >> char '\\'
        cod = undefined

pFloat :: Parser Double
pFloat = undefined

pInt :: Parser Integer
pInt = do
  s <- (negate <$ char '-' <|> id <$ char '+' <|> pure id)
  n <- read <$> many1 digit
  return (s n)

pAtom :: Parser Atom
pAtom =  AInt    <$> pInt
     <|> AFloat  <$> pFloat
     <|> AToken  <$> pToken
     <|> AString <$> pString

escape :: Char -> Text
escape '\n' = "\\n"
escape '\t' = "\\t"
escape '\r' = "\\r"
escape '\b' = "\\b"
escape '\f' = "\\f"
escape '\\' = "\\\\"
escape '"'  = "\\\""
escape c    = singleton c

sAtom :: Atom -> Text
sAtom (AToken t)  = t
sAtom (AString s) = "\"" <> concatMap escape s <> "\""
sAtom (AInt i)    = pack (show i)
sAtom (AFloat f)  = pack (show f)

fooSpec :: SExprSpec Atom (SExpr Atom)
fooSpec = mkSpec pAtom sAtom
