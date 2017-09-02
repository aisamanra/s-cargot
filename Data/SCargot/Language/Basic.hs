{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Language.Basic
  ( -- * Spec
    -- $descr
    basicParser
  , basicPrinter
  , locatedBasicParser
  ) where

import           Control.Applicative ((<$>))
import           Data.Char (isAlphaNum)
import           Text.Parsec (many1, satisfy)
import           Data.Text (Text, pack)
import           Data.Functor.Identity (Identity)
import           Text.Parsec.Prim (ParsecT)

import           Data.SCargot.Common (Located, located)
import           Data.SCargot.Repr.Basic (SExpr)
import           Data.SCargot ( SExprParser
                              , SExprPrinter
                              , mkParser
                              , flatPrint
                              )

isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c
  || c == '-' || c == '*' || c == '/'
  || c == '+' || c == '<' || c == '>'
  || c == '=' || c == '!' || c == '?'

pToken :: ParsecT Text a Identity Text
pToken = pack <$> many1 (satisfy isAtomChar)

-- $descr
-- The 'basicSpec' describes S-expressions whose atoms are simply
-- text strings that contain alphanumeric characters and a small
-- set of punctuation. It does no parsing of numbers or other data
-- types, and will accept tokens that typical Lisp implementations
-- would find nonsensical (like @77foo@).
--
-- Atoms recognized by the 'basicSpec' are any string matching the
-- regular expression @[A-Za-z0-9+*<>/=!?-]+@.

-- | A 'SExprParser' that understands atoms to be sequences of
--   alphanumeric characters as well as the punctuation
--   characters @[-*/+<>=!?]@, and does no processing of them.
--
-- >>> decode basicParser "(1 elephant)"
-- Right [SCons (SAtom "1") (SCons (SAtom "elephant") SNil)]
basicParser :: SExprParser Text (SExpr Text)
basicParser = mkParser pToken

-- | A 'basicParser' which produces 'Located' values
--
-- >>> decode locatedBasicParser $ pack "(1 elephant)"
-- Right [SCons (SAtom (At (Span (line 1, column 2) (line 1, column 3)) "1")) (SCons (SAtom (At (Span (line 1, column 4) (line 1, column 12)) "elephant")) SNil)]
--
-- >>> decode locatedBasicParser $ pack "(let ((x 1))\n  x)"
-- Right [SCons (SAtom (At (Span (line 1, column 2) (line 1, column 5)) "let")) (SCons (SCons (SCons (SAtom (At (Span (line 1, column 8) (line 1, column 9)) "x")) (SCons (SAtom (At (Span (line 1, column 10) (line 1, column 11)) "1")) SNil)) SNil) (SCons (SAtom (At (Span (line 2, column 3) (line 2, column 4)) "x")) SNil))]
locatedBasicParser :: SExprParser (Located Text) (SExpr (Located Text))
locatedBasicParser = mkParser $ located pToken

-- | A 'SExprPrinter' that prints textual atoms directly (without quoting
--   or any other processing) onto a single line.
--
-- >>> encode basicPrinter [L [A "1", A "elephant"]]
-- "(1 elephant)"
basicPrinter :: SExprPrinter Text (SExpr Text)
basicPrinter = flatPrint id
