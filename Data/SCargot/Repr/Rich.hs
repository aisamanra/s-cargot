{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.Rich
       ( -- * 'RichSExpr' representation
         R.RichSExpr(..)
       , R.toRich
       , R.fromRich
         -- * Useful pattern synonyms
       , pattern (:::)
       , pattern A
       , pattern L
       , pattern DL
       , pattern Nil
         -- * Useful processing functions
       , fromPair
       , fromList
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.SCargot.Repr as R

-- | A shorter infix alias to grab the head
--   and tail of an `RSList`.
pattern x ::: xs = R.RSList (x : xs)

-- | A shorter alias for `RSAtom`
pattern A a       = R.RSAtom a

-- | A shorter alias for `RSList`
pattern L xs      = R.RSList xs

-- | A shorter alias for `RSDotted`
pattern DL xs x = R.RSDotted xs x

-- | A shorter alias for `RSList []`
pattern Nil = R.RSList []

type S t = R.RichSExpr t
type Parse t a = S t -> Either String a

-- | Utility function for parsing a pair of things.
fromPair :: Parse t a -> Parse t b -> Parse t (a, b)
fromPair pl pr = asPair $ \(l,r) -> (,) <$> pl l <*> pr r

-- | Utility function for parsing a list of things.
fromList :: Parse t a -> Parse t [a]
fromList p = asList $ \ss -> mapM p ss

asPair :: ((S t, S t) -> Either String a) -> S t -> Either String a
asPair f (L [l, r]) = f (l, r)
asPair _ sx         = fail ("Expected two-element list")

asList :: ([S t] -> Either String a) -> S t -> Either String a
asList f (L ls) = f ls
asList _ sx     = fail ("Expected list")

asSymbol :: (t -> Either String a) -> S t -> Either String a
asSymbol f (A s) = f s
asSymbol _ sx    = fail ("Expected symbol")

asAssoc :: ([(S t, S t)] -> Either String a) -> S t -> Either String a
asAssoc f (L ss) = gatherPairs ss >>= f
  where gatherPairs (L [a, b] : ss) = (:) <$> pure (a, b) <*> gatherPairs ss
        gatherPairs []              = pure []
        gatherPairs _               = fail "..."
asAssoc _ sx     = fail ("Expected assoc list")
