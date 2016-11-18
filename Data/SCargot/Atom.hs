module Data.SCargot.Atom
  ( atom
  , mkAtomParser
  ) where

import Data.SCargot.Parse (SExprParser, mkParser)
import Data.SCargot.Repr (SExpr)
import Text.Parsec (choice)
import Text.Parsec.Text (Parser)

-- | A convenience function for defining an atom parser from a wrapper
--   function and a parser. This is identical to 'fmap' specialized to
--   operate over 'Parser' values, and is provided as sugar.
atom :: (t -> atom) -> Parser t -> Parser atom
atom = fmap

-- | A convenience function for defining a 'SExprSpec' from a list of
--   possible atom parsers, which will be tried in sequence before failing.
mkAtomParser :: [Parser atom] -> SExprParser atom (SExpr atom)
mkAtomParser = mkParser . choice
