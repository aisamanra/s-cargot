{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Basic
  ( basicSpec
  , asRich
  , asWellFormed
  , addReader
  , setComment
  , withSemicolonComments
  , withQuote
  ) where

import           Data.Char (isAlphaNum)
import           Data.Attoparsec.Text (Parser, takeWhile1)
import           Data.Text (Text)

import           Data.SCargot.Repr.Basic
import           Data.SCargot.General hiding (withQuote)

isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c
               || c == '-'
               || c == '*'
               || c == '/'
               || c == '+'
               || c == '<'
               || c == '>'
               || c == '='
               || c == '!'
               || c == '?'

-- | A 'SExprSpec' that understands atoms to be sequences of
--   alphanumeric characters as well as the punctuation
--   characters @-*/+<>=!?@, and does no processing of them.
--   This is not quite representative of actual lisps, which
--   would (for example) accept various kinds of string
--   literals. This should be sufficient for most ad-hoc
--   storage or configuration formats.
basicSpec :: SExprSpec Text (SExpr Text)
basicSpec = mkSpec (takeWhile1 isAtomChar) id

-- | Add the ability to understand a quoted S-Expression.
--   This means that @'sexpr@ becomes sugar for
--   @(quote sexpr)@. This is a variation on the identically-named
--   function in Data.SCargot.General that has been specialized
--   for the Basic atom type.
withQuote :: SExprSpec Text a -> SExprSpec Text a
withQuote = addReader '\'' (fmap go)
  where go s = SCons (SAtom "quote") (SCons s SNil)
