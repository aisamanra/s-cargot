{-# LANGUAGE PatternSynonyms #-}

module Data.SExpression.Repr.Rich
       ( pattern List
       , pattern Atom
       ) where

import Data.SExpression.Repr as R

pattern List xs = R.WFSList xs
pattern Atom a  = R.WFSAtom a
