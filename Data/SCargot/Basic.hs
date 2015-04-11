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

import           Control.Applicative ((<$>))
import           Data.Char (isAlphaNum)
import           Text.Parsec (many1, satisfy)
import           Data.Text (Text, pack)

import           Data.SCargot.Repr.Basic (SExpr)
import           Data.SCargot.General ( SExprSpec
                                      , mkSpec
                                      , asRich
                                      , asWellFormed
                                      , addReader
                                      , setComment
                                      , withQuote
                                      )
import           Data.SCargot.Comments (withLispComments)

isAtomChar :: Char -> Bool
isAtomChar c = isAlphaNum c
  || c == '-' || c == '*' || c == '/'
  || c == '+' || c == '<' || c == '>'
  || c == '=' || c == '!' || c == '?'

-- | A 'SExprSpec' that understands atoms to be sequences of
--   alphanumeric characters as well as the punctuation
--   characters @[-*/+<>=!?]@, and does no processing of them.
--   This is not quite representative of actual lisps, which
--   would, for example, accept various kinds of string
--   and numeric literals.
basicSpec :: SExprSpec Text (SExpr Text)
basicSpec = mkSpec pToken id
  where pToken = pack <$> many1 (satisfy isAtomChar)
