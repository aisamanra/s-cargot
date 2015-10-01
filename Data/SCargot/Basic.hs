{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Basic
  ( -- * Spec
    -- $descr
    basicSpec
  ) where

import           Control.Applicative ((<$>))
import           Data.Char (isAlphaNum)
import           Text.Parsec (many1, satisfy)
import           Data.Text (Text, pack)

import           Data.SCargot.Repr.Basic (SExpr)
import           Data.SCargot.General ( SExprSpec
                                      , mkSpec
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

-- | A 'SExprSpec' that understands atoms to be sequences of
--   alphanumeric characters as well as the punctuation
--   characters @[-*/+<>=!?]@, and does no processing of them.
basicSpec :: SExprSpec Text (SExpr Text)
basicSpec = mkSpec pToken id
  where pToken = pack <$> many1 (satisfy isAtomChar)
