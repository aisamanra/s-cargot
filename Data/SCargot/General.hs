module Data.SCargot.General
  ( -- * SExprSpec
    SExprSpec
  , mkSpec
  , convertSpec
  , addReader
  , addCommentType
  , asRich
  , asWellFormed
    -- * A Few Standard Reader Macros
  , quote
  , vector
    -- * Using a SExprSpec
  , parseSExpr
  , serializeSExpr
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Map.String (Map)
import qualified Data.Map.String as M

import           Data.SCargot.Repr

type ReaderMacroMap atom = Map Char (Reader atom)
type CommentMap = Map Char (Parser ())
type Reader atom = (Parser (SExpr atom) -> Parser (SExpr atom))
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
  , commentMap :: CommentMap
  , postparse  :: SExpr atom -> Either String carrier
  , preserial  :: carrier -> SExpr atom
  }

-- | This creates a basic 'SExprSpec' when given a parser and serializer
--   for an atom type.
mkSpec :: Parser atom -> Serializer atom -> SExprSpec atom (SExpr atom)
mkSpec p s = SExprSpec
  { sesPAtom  = p
  , sesSAtom  = s
  , rmMap     = M.empty
  , postparse = return
  , preserial = id
  }

-- | This is used to modify the carrier type for a 'SExprSpec'. This is
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
  , preserial = g . preserial spec
  }

addReader :: Char -> Reader a -> SExprSpec a c -> SExprSpec a c
addReader c reader spec = spec { rmMap = insert c reader (rmMap spec) }

addCommentType :: Char -> Comment -> SExprSpec a c -> SExprSpec a c
addCommentType c comment spec = spec { }

quote :: atom -> Reader atom
quote q parse = go <$> parse
  where go v = SCons q (SCons v SNil)

asRich :: SExprSpec a (SExpr b) -> SExprSpec a (RichSExpr b)
asRich = convertSpec (return . toRich) fromRich

asWellFormed :: SExprSpec a (SExpr b) -> SExprSpec a (WellFormedSExpr b)
asWellFormed = convertSpec toWellFormed fromWellFormed

parseGenericSExpr :: Parser atom  -> ReaderMacroMap atom -> CommentMap -> Parser (SExpr atom)
parseGenericSExpr atom reader comment =
  char '(' *> 

-- |
parseSExpr :: SExprSpec atom carrier -> Text -> Either String carrier
parseSExpr spec = undefined

-- | blah
serializeSExpr :: SExprSpec atom carrier -> carrier -> Text
serializeSExpr spec = serializeGenericSExpr ses . preserial
