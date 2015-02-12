{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.Rich
       ( pattern List
       , pattern DotList
       , pattern Atom
       ) where

import Data.SCargot.Repr as R

pattern List xs = R.RSList xs
pattern DotList xs = R.RSDotted xs
pattern Atom a = R.RSAtom a
