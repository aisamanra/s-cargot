{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Comments
  ( -- $intro

    -- * Lisp-Style Syntax

    -- $lisp
    withLispComments
    -- * Other Existing Comment Syntaxes
    -- ** Scripting Language Syntax
    -- $script
  , withOctothorpeComments
    -- ** C-Style Syntax
    -- $clike
  , withCLikeLineComments
  , withCLikeBlockComments
  , withCLikeComments
    -- ** Haskell-Style Syntax
    -- $haskell
  , withHaskellLineComments
  , withHaskellBlockComments
  , withHaskellComments
    -- * Comment Syntax Helper Functions
  , lineComment
  , simpleBlockComment
  ) where

import           Control.Monad (void)
import           Data.Text (Text)
import           Text.Parsec ( (<|>)
                             , anyChar
                             , manyTill
                             , noneOf
                             , skipMany
                             , string
                             )

import            Data.SCargot.Parse ( Comment
                                     , SExprParser
                                     , setComment
                                     )

-- | Given a string, produce a comment parser that matches that
--   initial string and ignores everything until the end of the
--   line.
lineComment :: String -> Comment
lineComment s = string s >> skipMany (noneOf "\n") >> return ()

-- | Given two strings, a begin and an end delimeter, produce a
--   parser that matches the beginning delimeter and then ignores
--   everything until it finds the end delimiter. This does not
--   consider nesting, so, for example, a comment created with
--
-- > curlyComment :: Comment
-- > curlyComment = simpleBlockComment "{" "}"
--
-- will consider
--
-- > { this { comment }
--
-- to be a complete comment, despite the apparent improper nesting.
-- This is analogous to standard C-style comments in which
--
-- > /* this /* comment */
--
-- is a complete comment.
simpleBlockComment :: String -> String -> Comment
simpleBlockComment begin end =
  string begin >>
  manyTill anyChar (string end) >>
  return ()

-- | Lisp-style line-oriented comments start with @;@ and last
--   until the end of the line. This is usually the comment
--   syntax you want.
withLispComments :: SExprParser t a -> SExprParser t a
withLispComments = setComment (lineComment ";")

-- | C++-like line-oriented comment start with @//@ and last
--   until the end of the line.
withCLikeLineComments :: SExprParser t a -> SExprParser t a
withCLikeLineComments = setComment (lineComment "//")

-- | C-like block comments start with @/*@ and end with @*/@.
--   They do not nest.
withCLikeBlockComments :: SExprParser t a -> SExprParser t a
withCLikeBlockComments = setComment (simpleBlockComment "/*" "*/")

-- | C-like comments include both line- and block-comments, the
--   former starting with @//@ and the latter contained within
--   @//* ... *//@.
withCLikeComments :: SExprParser t a -> SExprParser t a
withCLikeComments = setComment (lineComment "//" <|>
                                simpleBlockComment "/*" "*/")

-- | Haskell line-oriented comments start with @--@ and last
--   until the end of the line.
withHaskellLineComments :: SExprParser t a -> SExprParser t a
withHaskellLineComments = setComment (lineComment "--")

-- | Haskell block comments start with @{-@ and end with @-}@.
--   They do not nest.
withHaskellBlockComments :: SExprParser t a -> SExprParser t a
withHaskellBlockComments = setComment (simpleBlockComment "{-" "-}")

-- | Haskell comments include both the line-oriented @--@ comments
--   and the block-oriented @{- ... -}@ comments
withHaskellComments :: SExprParser t a -> SExprParser t a
withHaskellComments = setComment (lineComment "--" <|>
                                  simpleBlockComment "{-" "-}")

-- | Many scripting and shell languages use these, which begin with
--   @#@ and last until the end of the line.
withOctothorpeComments :: SExprParser t a -> SExprParser t a
withOctothorpeComments = setComment (lineComment "#")


{- $intro

By default a 'SExprParser' will not understand any kind of comment
syntax. Most varieties of s-expression will, however, want some kind
of commenting capability, so the below functions will produce a new
'SExprParser' which understands various kinds of comment syntaxes.

For example:

> mySpec :: SExprParser Text (SExpr Text)
> mySpec = asWellFormed $ mkParser (pack <$> many1 alphaNum)
>
> myLispySpec :: SExprParser Text (SExpr Text)
> myLispySpec = withLispComments mySpec
>
> myCLikeSpec :: SExprParser Text (SExpr Text)
> myCLikeSpec = withCLikeComment mySpec

We can then use these to parse s-expressions with different kinds of
comment syntaxes:

>>> decode mySpec "(foo ; a lisp comment\n  bar)\n"
Left "(line 1, column 6):\nunexpected \";\"\nexpecting space or atom"
>>> decode myLispySpec "(foo ; a lisp comment\n  bar)\n"
Right [WFSList [WFSAtom "foo", WFSAtom "bar"]]
>>> decode mySpec "(foo /* a c-like\n   comment */ bar)\n"
Left "(line 1, column 6):\nunexpected \"/\"\nexpecting space or atom"
>>> decode myCLikeSpec "(foo /* a c-like\n   comment */ bar)\n"
Right [WFSList [WFSAtom "foo", WFSAtom "bar"]]

-}

{- $lisp
> (one   ; a comment
>   two  ; another one
>   three)
-}

{- $script
> (one   # a comment
>   two  # another one
>   three)
-}

{- $clike
> (one // a comment
>   two /* another
>          one */
>   three)
-}

-- $haskell
-- > (one -- a comment
-- >   two {- another
-- >          one -}
-- >   three)
