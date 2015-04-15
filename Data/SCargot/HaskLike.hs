{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.HaskLike ( -- $info
                               haskLikeSpec
                             , HaskLikeAtom(..)
                             ) where

import           Control.Applicative ((<$>), (<*>), (<$))
import           Data.Maybe (catMaybes)
import           Data.String (IsString(..))
import           Data.Text (Text, pack)
import           Text.Parsec
import           Text.Parsec.Text (Parser)

import           Prelude hiding (concatMap)

import Data.SCargot.Repr.Basic (SExpr)
import Data.SCargot.General (SExprSpec, mkSpec)

{- $info

This module is intended for simple, ad-hoc configuration or data formats
that might not need their on rich structure but might benefit from a few
various literal formats. the 'haskLikeSpec' understands identifiers as
defined by R6RS as well as string, integer, and floating-point literals
as defined by the Haskell spec, but won't get any Lisp-specific vector
literals or other structure.

-}


-- | An atom type that understands Haskell-like values as well as
--   Scheme-like identifiers.
data HaskLikeAtom
  = HSIdent  Text  -- ^ An identifier, parsed according to the R6RS Scheme
                   --   standard
  | HSString Text  -- ^ A string, parsed according to the syntax for string
                   --   literals in the Haskell report
  | HSInt Integer  -- ^ An arbitrary-sized integer value, parsed according to
                   --   the syntax for integer literals in the Haskell report
  | HSFloat Double -- ^ A double-precision floating-point value, parsed
                   --   according to the syntax for floats in the Haskell
                   --   report
    deriving (Eq, Show)

instance IsString HaskLikeAtom where
  fromString = HSIdent . fromString

pToken :: Parser Text
pToken =  pack <$> (  (:) <$> initial <*> many subsequent
                  <|> string "+"
                  <|> string "-"
                  <|> string "..."
                   )

initial :: Parser Char
initial = letter <|> oneOf "!$%&*/:<=>?^_~"

subsequent :: Parser Char
subsequent = initial <|> digit <|> oneOf "+-.@"

pString :: Parser Text
pString = pack . catMaybes <$> between (char '"') (char '"') (many (val <|> esc))
  where val = Just <$> satisfy (\ c -> c /= '"' && c /= '\\' && c > '\026')
        esc = do char '\\'
                 Nothing <$ (gap <|> char '&') <|>
                   Just <$> code
        gap  = many1 space >> char '\\'
        code = eEsc <|> eNum <|> eCtrl <|> eAscii
        eCtrl  = char '^' >> unCtrl <$> upper
        eNum   = (toEnum . fromInteger) <$>
                   (decimal <|> (char 'o' >> number 8 octDigit)
                            <|> (char 'x' >> number 16 hexDigit))
        eEsc   = choice [ char a >> return b | (a, b) <- escMap ]
        eAscii = choice [ try (string a >> return b)
                        | (a, b) <- asciiMap ]
        unCtrl c = toEnum (fromEnum c - fromEnum 'A' + 1)

escMap :: [(Char,  Char)]
escMap = zip "abfntv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap = zip
  ["BS","HT","LF","VT","FF","CR","SO","SI","EM"
  ,"FS","GS","RS","US","SP","NUL","SOH","STX","ETX"
  ,"EOT","ENQ","ACK","BEL","DLE","DC1","DC2","DC3"
  ,"DC4","NAK","SYN","ETB","CAN","SUB","ESC","DEL"]
  ("\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP\NUL\SOH" ++
   "\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK" ++
   "\SYN\ETB\CAN\SUB\ESC\DEL")

decimal :: Parser Integer
decimal = number 10 digit

number :: Integer -> Parser Char -> Parser Integer
number base digits = foldl go 0 <$> many1 digits
  where go x d = base * x + toInteger (value d)
        value c
          | c == 'a' || c == 'A' = 0xa
          | c == 'b' || c == 'B' = 0xb
          | c == 'c' || c == 'C' = 0xc
          | c == 'd' || c == 'D' = 0xd
          | c == 'e' || c == 'E' = 0xe
          | c == 'f' || c == 'F' = 0xf
          | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
          | otherwise = error ("Unknown letter in number: " ++ show c)

pFloat :: Parser Double
pFloat = fail "???"

pInt :: Parser Integer
pInt = do
  s <- negate <$ char '-' <|> id <$ char '+' <|> return id
  n <- pZeroNum <|> decimal
  return (s n)

pZeroNum :: Parser Integer
pZeroNum = char '0' >>
  (  (oneOf "xX" >> number 16 hexDigit)
 <|> (oneOf "oO" >> number 8 octDigit)
 <|> decimal
 <|> return 0
  )

pHaskLikeAtom :: Parser HaskLikeAtom
pHaskLikeAtom =
      HSInt    <$> (try pInt   <?> "integer")
  <|> HSFloat  <$> (try pFloat <?> "float")
  <|> HSString <$> (pString    <?> "string literal")
  <|> HSIdent  <$> (pToken     <?> "token")

sHaskLikeAtom :: HaskLikeAtom -> Text
sHaskLikeAtom (HSIdent t)  = t
sHaskLikeAtom (HSString s) = pack (show s)
sHaskLikeAtom (HSInt i)    = pack (show i)
sHaskLikeAtom (HSFloat f)  = pack (show f)

-- | This `SExprSpec` understands s-expressions that contain
--   Scheme-like tokens, as well as string literals, integer
--   literals, and floating-point literals. These are read
--   and shown with Haskell lexical syntax, so the same set
--   of values understood by GHC should be understood by this
--   spec as well. This includes string escapes, different
--   number bases, and so forth.
haskLikeSpec :: SExprSpec HaskLikeAtom (SExpr HaskLikeAtom)
haskLikeSpec = mkSpec pHaskLikeAtom sHaskLikeAtom
