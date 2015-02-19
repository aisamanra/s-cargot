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
       ) where

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
