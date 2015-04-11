{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Basic
  ( basicSpec
  , asRich
  , asWellFormed
  , addReader
  , setComment
  , withLispComments
  , withQuote
  ) where

import           Data.Char (isAlphaNum)
import           Data.Attoparsec.Text (Parser, takeWhile1)
import           Data.Text (Text)

import           Data.SCargot.Repr.Basic
import           Data.SCargot.General
import           Data.SCargot.Comments (withLispComments)

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
