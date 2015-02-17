{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.Rich
       ( -- * 'RichSExpr' representation
         R.RichSExpr(..)
       , R.toRich
       , R.fromRich
         -- * Useful pattern synonyms
       , pattern List
       , pattern DotList
       , pattern Atom
       ) where

import Data.SCargot.Repr as R

pattern Atom a       = R.RSAtom a
pattern List xs      = R.RSList xs
pattern DotList xs x = R.RSDotted xs x
