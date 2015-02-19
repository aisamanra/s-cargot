{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.Basic
       ( -- * Basic 'SExpr' representation
         R.SExpr(..)
         -- * Shorthand Patterns
       , pattern (:::)
       , pattern A
       , pattern Nil
       ) where

import Data.SCargot.Repr as R

-- | A shorter infix alias for `SCons`
pattern x ::: xs = SCons x xs

-- | A shorter alias for `SAtom`
pattern A x = SAtom x

-- | A (slightly) shorter alias for `SNil`
pattern Nil = SNil
