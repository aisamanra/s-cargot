{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SCargot.Repr
       ( -- $reprs
         -- * Elementary SExpr representation
         SExpr(..)
         -- * Rich SExpr representation
       , RichSExpr(..)
       , toRich
       , fromRich
         -- * Well-Formed SExpr representation
       , WellFormedSExpr(..)
       , toWellFormed
       , fromWellFormed
       ) where

import Data.Data (Data)
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import GHC.Exts (IsList(..), IsString(..))

#if !MIN_VERSION_base(4,8,0)
import Prelude hiding (foldr)
#endif

-- | All S-Expressions can be understood as a sequence
--   of @cons@ cells (represented here by 'SCons'), the
--   empty list @nil@ (represented by 'SNil') or an
--   @atom@.
data SExpr atom
  = SCons (SExpr atom) (SExpr atom)
  | SAtom atom
  | SNil
    deriving (Eq, Show, Read, Functor, Data, Typeable, Foldable, Traversable)

instance IsString atom => IsString (SExpr atom) where
  fromString = SAtom . fromString

instance IsList (SExpr atom) where
  type Item (SExpr atom) = SExpr atom
  fromList = foldr SCons SNil
  toList   = go
    where go (SCons x xs) = x : go xs
          go SNil         = []
          go (SAtom {})   = error "Unable to turn atom into list"

-- | Sometimes the cons-based interface is too low
--   level, and we'd rather have the lists themselves
--   exposed. In this case, we have 'RSList' to
--   represent a well-formed cons list, and 'RSDotted'
--   to represent an improper list of the form
--   @(a b c . d)@. This representation is based on
--   the structure of the parsed S-Expression, and not on
--   how it was originally represented: thus, @(a . (b))@ is going to
--   be represented as @RSList[RSAtom a, RSAtom b]@
--   despite having been originally represented as a
--   dotted list.
data RichSExpr atom
  = RSList [RichSExpr atom]
  | RSDotted [RichSExpr atom] atom
  | RSAtom atom
    deriving (Eq, Show, Read, Functor, Data, Typeable, Foldable, Traversable)

instance IsString atom => IsString (RichSExpr atom) where
  fromString = RSAtom . fromString

instance IsList (RichSExpr atom) where
  type Item (RichSExpr atom) = RichSExpr atom
  fromList = RSList
  toList (RSList xs)   = xs
  toList (RSDotted {}) = error "Unable to turn dotted list into haskell list"
  toList (RSAtom {})   = error "Unable to turn atom into Haskell list"

-- |  It should always be true that
--
--   > fromRich (toRich x) == x
--
--   and that
--
--   > toRich (fromRich x) == x
toRich :: SExpr atom -> RichSExpr atom
toRich (SAtom a) = RSAtom a
toRich (SCons x xs) = go xs (toRich x:)
  where go (SAtom a) rs    = RSDotted (rs []) a
        go SNil rs         = RSList (rs [])
        go (SCons y ys) rs = go ys (rs . (toRich y:))
toRich SNil = RSList []

-- | This follows the same laws as 'toRich'.
fromRich :: RichSExpr atom -> SExpr atom
fromRich (RSAtom a) = SAtom a
fromRich (RSList xs) = foldr SCons SNil (map fromRich xs)
fromRich (RSDotted xs x) = foldr SCons (SAtom x) (map fromRich xs)

-- | A well-formed s-expression is one which does not
--   contain any dotted lists. This means that not
--   every value of @SExpr a@ can be converted to a
--   @WellFormedSExpr a@, although the opposite is
--   fine.
data WellFormedSExpr atom
  = WFSList [WellFormedSExpr atom]
  | WFSAtom atom
    deriving (Eq, Show, Read, Functor, Data, Typeable, Foldable, Traversable)

instance IsList (WellFormedSExpr atom) where
  type Item (WellFormedSExpr atom) = WellFormedSExpr atom
  fromList = WFSList
  toList (WFSList xs) = xs
  toList (WFSAtom {}) = error "Unable to turn atom into Haskell list"

instance IsString atom => IsString (WellFormedSExpr atom) where
  fromString = WFSAtom . fromString

-- | This will be @Nothing@ if the argument contains an
--   improper list. It should hold that
--
--   > toWellFormed (fromWellFormed x) == Right x
--
--   and also (more tediously) that
--
--   > case toWellFormed x of
--   >   Left _  -> True
--   >   Right y -> x == fromWellFormed y
toWellFormed :: SExpr atom -> Either String (WellFormedSExpr atom)
toWellFormed SNil      = return (WFSList [])
toWellFormed (SAtom a) = return (WFSAtom a)
toWellFormed (SCons x xs) = do
  x' <- toWellFormed x
  go xs (x':)
  where go (SAtom _) _  = Left "Found atom in cdr position"
        go SNil rs      = return (WFSList (rs []))
        go (SCons y ys) rs = do
          y' <- toWellFormed y
          go ys (rs . (y':))

-- | Convert a WellFormedSExpr back into a SExpr.
fromWellFormed :: WellFormedSExpr atom -> SExpr atom
fromWellFormed (WFSAtom a)  = SAtom a
fromWellFormed (WFSList xs) =
  foldr SCons SNil (map fromWellFormed xs)

{- $reprs

This module contains several different representations for
s-expressions. The s-cargot library underlying uses the
'SExpr' type as its representation type, which is a binary
tree representation with an arbitrary type for its leaves.

This type is not always convenient to manipulate in Haskell
code, this module defines two alternate representations
which turn a sequence of nested right-branching cons pairs
into Haskell lists: that is to say, they transform between

@
SCons a (SCons b (SCons c SNil))  \<=\>  RSList [a, b, c]
@

These two types differ in how they handle non-well-formed
lists, i.e. lists that end with an atom. The 'RichSExpr'
format handles this with a special constructor for lists
that end in an atom:

@
SCons a (SCons b (SAtom c))  \<=\>  RSDotted [a, b] c
@

On the other hand, the 'WellFormedSExpr' type elects
not to handle this case. This is unusual for Lisp source code,
but is a reasonable choice for configuration or data
storage formats that use s-expressions, where
non-well-formed lists would be an unnecessary
complication.

To make working with these types less verbose, there are other
modules that export pattern aliases and helper functions: these
can be found at "Data.SCargot.Repr.Basic",
"Data.SCargot.Repr.Rich", and "Data.SCargot.Repr.WellFormed".
-}
