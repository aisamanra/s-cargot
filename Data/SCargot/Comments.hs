{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Comments
  ( -- * Comment Syntax
    -- $intro
    -- * Lisp Comments
    withLispComments
    -- * Other Existing Comment Syntaxes
    -- ** Scripting Language Syntax
    -- $script
  , withOctothorpeComments
    -- ** C-Like Syntax
    -- $clike
  , withCLikeLineComments
  , withCLikeBlockComments
  , withCLikeComments
    -- ** Haskell Syntax
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

import Data.SCargot.General ( Comment
                            , SExprSpec
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
-- to be a complete comment, despite the improper nesting. This is
-- analogous to standard C-style comments in which
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
withLispComments :: SExprSpec t a -> SExprSpec t a
withLispComments = setComment (lineComment ";")

-- | C++-like line-oriented comment start with @//@ and last
--   until the end of the line.
withCLikeLineComments :: SExprSpec t a -> SExprSpec t a
withCLikeLineComments = setComment (lineComment "//")

-- | C-like block comments start with @/*@ and end with @*/@.
--   They do not nest.
withCLikeBlockComments :: SExprSpec t a -> SExprSpec t a
withCLikeBlockComments = setComment (simpleBlockComment "/*" "*/")

-- | C-like comments include both line- and block-comments, the
--   former starting with @//@ and the latter contained within
--   @//* ... *//@.
withCLikeComments :: SExprSpec t a -> SExprSpec t a
withCLikeComments = setComment (lineComment "//" <|>
                                simpleBlockComment "/*" "*/")

-- | Haskell line-oriented comments start with @--@ and last
--   until the end of the line.
withHaskellLineComments :: SExprSpec t a -> SExprSpec t a
withHaskellLineComments = setComment (lineComment "--")

-- | Haskell block comments start with @{-@ and end with @-}@.
--   They do not nest.
withHaskellBlockComments :: SExprSpec t a -> SExprSpec t a
withHaskellBlockComments = setComment (simpleBlockComment "{-" "-}")

-- | Haskell comments include both the line-oriented @--@ comments
--   and the block-oriented @{- ... -}@ comments
withHaskellComments :: SExprSpec t a -> SExprSpec t a
withHaskellComments = setComment (lineComment "--" <|>
                                  simpleBlockComment "{-" "-}")

-- | Many scripting and shell languages use these, which begin with
--   @#@ and last until the end of the line.
withOctothorpeComments :: SExprSpec t a -> SExprSpec t a
withOctothorpeComments = setComment (lineComment "#")


{- $intro

By default a 'SExprSpec' will not understand any kind of comment
syntax. Most varieties of s-expression will, however, want some kind
of commenting capability, so the below functions will produce a new
'SExprSpec' which understands various kinds of comment syntaxes.

For example:

> mySpec :: SExprSpec Text (SExpr Text)
> mySpec = asWellFormed (mkSpec (takeWhile1 isAlphaNum) id)
>
> myLispySpec :: SExprSpec Text (SExpr Text)
> myLispySpec = withLispComments mySpec
>
> myCLikeSpec :: SExprSpec Text (SExpr Text)
> myCLikeSpec = withCLikeComment mySpec

We can then use these to parse s-expressions with different kinds of
comment syntaxes:

> > decode mySpec "(foo ; a lisp comment\n  bar)\n"
> Left "Failed reading: takeWhile1"
> > decode myLispySpec "(foo ; a lisp comment\n  bar)\n"
> Right [WFSList [WFSAtom "foo", WFSAtom "bar"]]
> > decode mySpec "(foo /* a c-like\n   comment */ bar)\n"
> Left "Failed reading: takeWhile1"
> > decode myCLikeSpec "(foo /* a c-like\n   comment */ bar)\n"
> Right [WFSList [WFSAtom "foo", WFSAtom "bar"]]

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
