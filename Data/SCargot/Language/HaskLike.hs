{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Language.HaskLike
  ( -- $info
    HaskLikeAtom(..)
  , haskLikeParser
  , haskLikePrinter
    -- * Individual Parsers
  , parseHaskellString
  , parseHaskellFloat
  , parseHaskellInt
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<$))
#endif
import           Data.Maybe (catMaybes)
import           Data.String (IsString(..))
import           Data.Text (Text, pack)
import           Text.Parsec
import           Text.Parsec.Text (Parser)

import           Prelude hiding (concatMap)

import Data.SCargot.Common
import Data.SCargot.Repr.Basic (SExpr)
import Data.SCargot (SExprParser, SExprPrinter, mkParser, flatPrint)

{- $info

This module is intended for simple, ad-hoc configuration or data formats
that might not need their on rich structure but might benefit from a few
various kinds of literals. The 'haskLikeParser' understands identifiers as
defined by R5RS, as well as string, integer, and floating-point literals
as defined by the Haskell spec. It does __not__ natively understand other
data types, such as booleans, vectors, bitstrings.

-}


-- | An atom type that understands Haskell-like values as well as
--   Scheme-like identifiers.
data HaskLikeAtom
  = HSIdent  Text  -- ^ An identifier, parsed according to the R5RS Scheme
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

parseHaskellString :: Parser Text
parseHaskellString = pack . catMaybes <$> between (char '"') (char '"') (many (val <|> esc))
  where val = Just <$> satisfy (\ c -> c /= '"' && c /= '\\' && c > '\026')
        esc = do _ <- char '\\'
                 Nothing <$ (gap <|> char '&') <|>
                   Just <$> code
        gap  = many1 space >> char '\\'
        code = eEsc <|> eNum <|> eCtrl <|> eAscii
        eCtrl  = char '^' >> unCtrl <$> upper
        eNum   = (toEnum . fromInteger) <$>
                   (decNumber <|> (char 'o' >> octNumber)
                              <|> (char 'x' >> hexNumber))
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

parseHaskellFloat :: Parser Double
parseHaskellFloat = do
  n <- decNumber
  withDot n <|> noDot n
  where withDot n = do
          _ <- char '.'
          m <- decNumber
          e <- option 1.0 expn
          return ((fromIntegral n + asDec m 0) * e)
        noDot n = do
          e <- expn
          return (fromIntegral n * e)
        expn = do
          _ <- oneOf "eE"
          s <- power
          x <- decNumber
          return (10 ** s (fromIntegral x))
        asDec 0 k = k
        asDec n k =
          asDec (n `div` 10) ((fromIntegral (n `rem` 10) + k) * 0.1)

power :: Num a => Parser (a -> a)
power = negate <$ char '-' <|> id <$ char '+' <|> return id

parseHaskellInt :: Parser Integer
parseHaskellInt = do
  s <- power
  n <- pZeroNum <|> decNumber
  return (fromIntegral (s n))

pZeroNum :: Parser Integer
pZeroNum = char '0' >>
  (  (oneOf "xX" >> hexNumber)
 <|> (oneOf "oO" >> octNumber)
 <|> decNumber
 <|> return 0
  )

pHaskLikeAtom :: Parser HaskLikeAtom
pHaskLikeAtom
   =  HSFloat   <$> (try parseHaskellFloat <?> "float")
  <|> HSInt     <$> (try parseHaskellInt   <?> "integer")
  <|> HSString  <$> (parseHaskellString    <?> "string literal")
  <|> HSIdent   <$> (parseR5RSIdent <?> "token")

sHaskLikeAtom :: HaskLikeAtom -> Text
sHaskLikeAtom (HSIdent t)  = t
sHaskLikeAtom (HSString s) = pack (show s)
sHaskLikeAtom (HSInt i)    = pack (show i)
sHaskLikeAtom (HSFloat f)  = pack (show f)

-- | This `SExprParser` understands s-expressions that contain
--   Scheme-like tokens, as well as string literals, integer
--   literals, and floating-point literals. Each of these values
--   is parsed according to the lexical rules in the Haskell
--   report, so the same set of string escapes, numeric bases,
--   and floating-point options are available. This spec does
--   not parse comments and does not understand any reader
--   macros.
--
-- >>> decode haskLikeParser "(0x01 \"\\x65lephant\")"
-- Right [SCons (SAtom (HSInt 1)) (SCons (SAtom (HSString "elephant")) SNil)]
haskLikeParser :: SExprParser HaskLikeAtom (SExpr HaskLikeAtom)
haskLikeParser = mkParser pHaskLikeAtom

-- | This 'SExprPrinter' emits s-expressions that contain Scheme-like
--   tokens as well as string literals, integer literals, and floating-point
--   literals, which will be emitted as the literals produced by Haskell's
--   'show' function. This printer will produce a flat s-expression with
--   no indentation of any kind.
--
-- >>> encode haskLikePrinter [L [A (HSInt 1), A (HSString "elephant")]]
-- "(1 \"elephant\")"
haskLikePrinter :: SExprPrinter HaskLikeAtom (SExpr HaskLikeAtom)
haskLikePrinter = flatPrint sHaskLikeAtom
