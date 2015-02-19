{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.WellFormed
       ( -- * 'WellFormedSExpr' representation
         R.WellFormedSExpr(..)
       , R.toWellFormed
       , R.fromWellFormed
         -- * Useful pattern synonyms
       , pattern (:::)
       , pattern L
       , pattern A
       , pattern Nil
       ) where

import Data.SCargot.Repr as R

-- | A shorter infix alias to grab the head
--   and tail of a `WFSList`
pattern x ::: xs = R.WFSList (x : xs)

-- | A shorter alias for `WFSList`
pattern L xs = R.WFSList xs

-- | A shorter alias for `WFSAtom`
pattern A a  = R.WFSAtom a

-- | A shorter alias for `WFSList []`
pattern Nil = R.WFSList []
