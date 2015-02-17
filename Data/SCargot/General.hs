{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.General
  ( -- * SExprSpec
    SExprSpec
  , mkSpec
  , convertSpec
  , addReader
  , addComment
    -- * Specific SExprSpec Conversions
  , asRich
  , asWellFormed
  , withSemicolonComments
  , withQuote
    -- * Using a SExprSpec
  , decode
  , decodeOne
  , encode
    -- * Useful Type Aliases
  , Reader
  , Comment
  , Serializer
  ) where

import           Control.Applicative ((<*))
import           Control.Monad ((>=>))
import           Data.Attoparsec.Text
import           Data.Char (isAlpha)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           Prelude hiding (takeWhile)

import           Data.SCargot.Repr

type ReaderMacroMap atom = Map Char (Reader atom)
type CommentMap = Map Char Comment

-- | A 'Reader' represents a reader macro: it takes a parser for
--   the S-Expression type and performs as much or as little
--   parsing as it would like, and then returns an S-expression.
type Reader atom = (Parser (SExpr atom) -> Parser (SExpr atom))

-- | A 'Comment' represents any kind of skippable comment.
type Comment = Parser ()

-- | A 'Serializer' is any function which can serialize an Atom
--   to 'Text'.
type Serializer atom = atom -> Text

-- | A 'SExprSpec' describes a parser and emitter for a particular
--   variant of S-Expressions. The @atom@ type corresponds to a
--   Haskell type used to represent the atoms, and the @carrier@
--   type corresponds to the parsed S-Expression structure. The
--   'SExprSpec' type is deliberately opaque so that it must be
--   constructed and modified with other helper functions.
data SExprSpec atom carrier = SExprSpec
  { sesPAtom   :: Parser atom
  , sesSAtom   :: Serializer atom
  , readerMap  :: ReaderMacroMap atom
  , comment    :: Comment
  , postparse  :: SExpr atom -> Either String carrier
  , preserial  :: carrier -> SExpr atom
  }

-- | Create a basic 'SExprSpec' when given a parser and serializer
--   for an atom type.
mkSpec :: Parser atom -> Serializer atom -> SExprSpec atom (SExpr atom)
mkSpec p s = SExprSpec
  { sesPAtom   = p
  , sesSAtom   = s
  , readerMap  = M.empty
  , commentMap = skipSpace
  , postparse  = return
  , preserial  = id
  }

-- | Modify the carrier type for a 'SExprSpec'. This is
--   used internally to convert between various 'SExpr' representations,
--   but could also be used externally to add an extra conversion layer
--   onto a 'SExprSpec', e.g. for a custom Lisp-like language:
--
--   > mySpec :: SExprSpec MyAtomType MyAST
--   > mySpec = convertSpec sexprToMyAST myASTToSexpr spec
--   >   where spec = mkSpec myParser mySerializer
convertSpec :: (b -> Either String c) -> (c -> b) -> SExprSpec a b -> SExprSpec a c
convertSpec f g spec = spec
  { postparse = postparse spec >=> f
  , preserial = preserial spec . g
  }

-- | Convert the final output representation from the 'SExpr' type
--   to the 'RichSExpr' type.
asRich :: SExprSpec a (SExpr b) -> SExprSpec a (RichSExpr b)
asRich = convertSpec (return . toRich) fromRich

-- | Convert the final output representation from the 'SExpr' type
--   to the 'WellFormedSExpr' type.
asWellFormed :: SExprSpec a (SExpr b) -> SExprSpec a (WellFormedSExpr b)
asWellFormed = convertSpec toWellFormed fromWellFormed

-- | Add the ability to execute some particular reader macro, as
--   defined by its initial character and the 'Parser' which returns
--   the parsed S-Expression. The 'Reader' is passed a 'Parser' which
--   can be recursively called to parse more S-Expressions, and begins
--   parsing after the reader character has been removed from the
--   stream.
addReader :: Char -> Reader a -> SExprSpec a c -> SExprSpec a c
addReader c reader spec = spec
  { readerMap = M.insert c reader (readerMap spec) }

-- | Add the ability to ignore some kind of comment. If the comment
--   parser overlaps with a reader macro or the atom parser, then the
--   former will be tried first.
setComment :: Comment -> SExprSpec a c -> SExprSpec a c
setComment c spec = spec { comment = c }

-- | Add the ability to skip line comments beginning with a semicolon.
withSemicolonComments :: SExprSpec a c -> SExprSpec a c
withSemicolonComments = addComment ';' (skipWhile (\ c -> c /= '\n'))

-- | Add the ability to understand a quoted S-Expression. In general,
--   many Lisps use @'sexpr@ as sugar for @(quote sexpr)@. This is
--   a convenience function which allows you to easily add quoted
--   expressions to a 'SExprSpec', provided that you supply which
--   atom you want substituted in for the symbol @quote@.
withQuote :: a -> SExprSpec a (SExpr a) -> SExprSpec a (SExpr a)
withQuote q = addReader '\'' prs
  where prs p = go `fmap` p
        go s  = SCons (SAtom q) (SCons s SNil)

parseGenericSExpr ::
  Parser atom  -> ReaderMacroMap atom -> Parser () -> Parser (SExpr atom)
parseGenericSExpr atom reader skip = do
  let sExpr = parseGenericSExpr atom reader skip
  skip
  c <- peekChar
  r <- case c of
    Nothing -> fail "Unexpected end of input"
    Just '(' -> char '(' >> skip >> parseList sExpr skip
    Just (flip M.lookup reader -> Just r) -> anyChar >> r sExpr
    _ -> SAtom `fmap` atom
  skip
  return r

parseList :: Parser (SExpr atom) -> Parser () -> Parser (SExpr atom)
parseList sExpr skip = do
  i <- peekChar
  case i of
    Nothing  -> fail "Unexpected end of input"
    Just ')' -> char ')' >> return SNil
    _        -> do
      car <- sExpr
      skip
      c <- peekChar
      case c of
        Just '.' -> do
          char '.'
          cdr <- sExpr
          skip
          char ')'
          skip
          return (SCons car cdr)
        Just ')' -> do
          char ')'
          skip
          return (SCons car SNil)
        _ -> do
          cdr <- parseList sExpr skip
          return (SCons car cdr)

-- | Given a CommentMap, create the corresponding parser to
--   skip those comments (if they exist).
buildSkip :: CommentMap -> Parser ()
buildSkip m = skipSpace >> comments >> skipSpace
  where comments = do
          c <- peekChar
          case c of
            Nothing -> return ()
            Just c' -> case M.lookup c' m of
              Just p  -> anyChar >> p
              Nothing -> return ()

(#) :: a -> (a -> b) -> b
(#) = flip ($)

testSpec :: SExprSpec Text (SExpr Text)
testSpec = mkSpec (takeWhile1 isAlpha) id
         # withQuote "quote"
         # addReader '#' (\ p -> SCons (SAtom "vector") `fmap` p)

-- | Decode a single S-expression. If any trailing input is left after
--   the S-expression (ignoring comments or whitespace) then this
--   will fail: for those cases, use 'decode', which returns a list of
--   all the S-expressions found at the top level.
decodeOne :: SExprSpec atom carrier -> Text -> Either String carrier
decodeOne SExprSpec { .. } = parseOnly (parser <* endOfInput) >=> postparse
  where parser = parseGenericSExpr sesPAtom readerMap (buildSkip commentMap)

-- | Decode several S-expressions according to a given 'SExprSpec'. This
--   will return a list of every S-expression that appears at the top-level
--   of the document.
decode :: SExprSpec atom carrier -> Text -> Either String [carrier]
decode SExprSpec { .. } =
  parseOnly (many1 parser <* endOfInput) >=> mapM postparse
    where parser = parseGenericSExpr sesPAtom readerMap (buildSkip commentMap)

-- | Emit an S-Expression in a machine-readable way. This
encode :: SExprSpec atom carrier -> carrier -> Text
encode SExprSpec { .. } = undefined
