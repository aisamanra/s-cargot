{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.Rich
       ( -- * 'WellFormedSExpr' representation
         R.WellFormedSExpr(..)
       , R.toWellFormed
       , R.fromWellFormed
         -- * Useful pattern synonyms
       , pattern List
       , pattern Atom
       ) where

import Data.SCargot.Repr as R

pattern List xs = R.WFSList xs
pattern Atom a  = R.WFSAtom a
