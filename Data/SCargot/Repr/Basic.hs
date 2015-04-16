{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.Basic
       ( -- * Basic 'SExpr' representation
         R.SExpr(..)
         -- * Shorthand Patterns
       , pattern (:::)
       , pattern A
       , pattern Nil
         -- * Useful processing functions
       , fromPair
       , fromList
       , fromAtom
       , asPair
       , asList
       , isAtom
       , asAtom
       , asAssoc
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.SCargot.Repr as R

-- | A shorter infix alias for `SCons`
pattern x ::: xs = SCons x xs

-- | A shorter alias for `SAtom`
pattern A x = SAtom x

-- | A (slightly) shorter alias for `SNil`
pattern Nil = SNil

-- | Utility function for parsing a pair of things.
fromPair :: (SExpr t -> Either String a)
         -> (SExpr t -> Either String b)
         -> SExpr t -> Either String (a, b)
fromPair pl pr (l ::: r ::: Nil) = (,) <$> pl l <*> pr r
fromPair _  _  sx = Left ("Expected two-element list")

-- | Utility function for parsing a list of things.
fromList :: (SExpr t -> Either String a) -> SExpr t -> Either String [a]
fromList p (s ::: ss) = (:) <$> p s <*> fromList p ss
fromList p Nil        = pure []
fromList _ sx         = Left ("Expected list")

-- | Utility function for parsing a single atom
fromAtom :: SExpr t -> Either String t
fromAtom (A a) = return a
fromAtom _     = Left "Expected atom; found list"

gatherList :: SExpr t -> Either String [SExpr t]
gatherList (x ::: xs) = (:) <$> pure x <*> gatherList xs
gatherList Nil        = pure []
gatherList sx         = Left ("Expected list")

-- | Parse a two-element list (NOT a dotted pair) using the
--   provided function.
asPair :: ((SExpr t, SExpr t) -> Either String a)
       -> SExpr t -> Either String a
asPair f (l ::: r ::: SNil) = f (l, r)
asPair _ sx = Left ("Expected two-element list")

-- | Parse an arbitrary-length list using the provided function.
asList :: ([SExpr t] -> Either String a) -> SExpr t -> Either String a
asList f ls = gatherList ls >>= f

-- | Match a given literal atom, failing otherwise.
isAtom :: Eq t => t -> SExpr t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = Left ".."
isAtom _ _ = Left ".."

-- | Parse an atom using the provided function.
asAtom :: (t -> Either String a) -> SExpr t -> Either String a
asAtom f (A s) = f s
asAtom _ sx    = Left ("Expected symbol")

-- | Parse an assoc-list using the provided function.
asAssoc :: ([(SExpr t, SExpr t)] -> Either String a)
        -> SExpr t -> Either String a
asAssoc f ss = gatherList ss >>= mapM go >>= f
  where go (a ::: b ::: Nil) = return (a, b)
        go sx = Left ("Expected two-element list")
