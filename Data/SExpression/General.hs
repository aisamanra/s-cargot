module Data.SExpression.General where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Map.String (Map)
import qualified Data.Map.String as M

type ReaderMacroMap atom = Map Char (Reader atom)
type Reader atom = (Parser (SExpr atom) -> Parser (SExpr atom))
type Serializer atom = atom -> Text

-- | A 'SExprSpec' describes a parser and emitter for a particular
--   variant of S-Expressions. The @atom@ type corresponds to a
--   Haskell type used to represent the atoms, and the @carrier@
--   type corresponds to the parsed S-Expression structure. This
--   is deliberately opaque so that it must be constructed and
--   modified with other helper functions.
data SExprSpec atom carrier = SExprSpec
  { sesPAtom  :: Parser atom
  , sesSAtom  :: Serializer atom
  , rmMap     :: ReaderMacroMap atom
  , postparse :: SExpr atom -> Either String carrier
  , preserial :: carrier -> SExpr atom
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

quote :: atom -> Reader atom
quote q parse = go <$> parse
  where go v = SCons q (SCons v SNil)

toRich :: SExprSpec a (SExpr b) -> SExprSpec a (RichSExpr b)
toRich = convertSpec (return . toRich) fromRich

toWellFormed :: SExprSpec a (SExpr b) -> SExprSpec a (WellFormedSExpr b)
toWellFormed = convertSpec toWellFormed fromWellFormed

parseGenericSExpr :: Parser atom  -> ReaderMacroMap atom -> Parser (SExpr atom)

-- |
parseSExpr :: SExprSpec atom carrier -> Text -> Either String carrier
parseSExpr spec = undefined

-- | blah
serializeSExpr :: SExprSpec atom carrier -> carrier -> Text
serializeSExpr spec = serializeGenericSExpr ses . preserial
