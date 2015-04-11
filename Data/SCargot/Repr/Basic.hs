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
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.SCargot.Repr as R

-- | A shorter infix alias for `SCons`
pattern x ::: xs = SCons x xs

-- | A shorter alias for `SAtom`
pattern A x = SAtom x

-- | A (slightly) shorter alias for `SNil`
pattern Nil = SNil


type S t = R.SExpr t
type Parse t a = R.SExpr t -> Either String a

-- | Utility function for parsing a pair of things.
fromPair :: Parse t a -> Parse t b -> Parse t (a, b)
fromPair pl pr (l ::: r ::: Nil) = (,) <$> pl l <*> pr r
fromPair _  _  sx = fail ("Expected two-element list")

-- | Utility function for parsing a list of things.
fromList :: Parse t a -> Parse t [a]
fromList p (s ::: ss) = (:) <$> p s <*> fromList p ss
fromList p Nil        = pure []
fromList _ sx         = fail ("Expected list")

gatherList :: S t -> Either String [S t]
gatherList (x ::: xs) = (:) <$> pure x <*> gatherList xs
gatherList Nil        = pure []
gatherList sx         = fail ("Expected list")

asPair :: ((S t, S t) -> Either String a) -> S t -> Either String a
asPair f (l ::: r ::: SNil) = f (l, r)
asPair _ sx = fail ("Expected two-element list")

asList :: ([S t] -> Either String a) -> S t -> Either String a
asList f ls = gatherList ls >>= f

asSymbol :: (t -> Either String a) -> S t -> Either String a
asSymbol f (A s) = f s
asSymbol _ sx    = fail ("Expected symbol")

asAssoc :: ([(S t, S t)] -> Either String a) -> S t -> Either String a
asAssoc f ss = gatherList ss >>= mapM go >>= f
  where go (a ::: b ::: Nil) = return (a, b)
        go sx = fail ("Expected two-element list")
