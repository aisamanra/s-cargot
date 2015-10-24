{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Language.Basic
  ( -- * Spec
    -- $descr
    basicParser
  , basicPrinter
  ) where

import           Control.Applicative ((<$>))
import           Data.Char (isAlphaNum)
import           Text.Parsec (many1, satisfy)
import           Data.Text (Text, pack)

import           Data.SCargot.Repr.Basic (SExpr)
import           Data.SCargot ( SExprParser
                              , SExprPrinter
                              , mkParser
                              , flatPrint
                              )
import           Data.SCargot.Comments (withLispComments)

isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c
  || c == '-' || c == '*' || c == '/'
  || c == '+' || c == '<' || c == '>'
  || c == '=' || c == '!' || c == '?'

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
  where pToken = pack <$> many1 (satisfy isAtomChar)

-- | A 'SExprPrinter' that prints textual atoms directly (without quoting
--   or any other processing) onto a single line.
--
-- >>> encode basicPrinter [L [A "1", A "elephant"]]
-- "(1 elephant)"
basicPrinter :: SExprPrinter Text (SExpr Text)
basicPrinter = flatPrint id
