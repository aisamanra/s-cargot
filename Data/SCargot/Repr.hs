{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SCargot.Repr
       ( -- * Elementary SExpr representation
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

import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..), (<>))
import Data.Traversable (Traversable(..))
import GHC.Exts (IsList(..), IsString(..))

-- | All S-Expressions can be understood as a sequence
--   of @cons@ cells (represented here by 'SCons'), the
--   empty list @nil@ (represented by 'SNil') or an
--   @atom@.
data SExpr atom
  = SCons (SExpr atom) (SExpr atom)
  | SAtom atom
  | SNil
    deriving (Eq, Show, Read, Functor, Data)

instance IsString atom => IsString (SExpr atom) where
  fromString = SAtom . fromString

instance IsList (SExpr atom) where
  type Item (SExpr atom) = SExpr atom
  fromList = foldr SCons SNil
  toList   = go
    where go (SCons x xs) = x : go xs
          go SNil         = []
          go (SAtom {})   = error "Unable to turn atom into list"

instance Foldable SExpr where
  foldMap _ SNil        = mempty
  foldMap f (SAtom a)   = f a
  foldMap f (SCons x y) = foldMap f x <> foldMap f y

instance Traversable SExpr where
  traverse f SNil        = pure SNil
  traverse f (SAtom a)   = SAtom <$> f a
  traverse f (SCons x y) = SCons <$> traverse f x <*> traverse f y

-- | Sometimes the cons-based interface is too low
--   level, and we'd rather have the lists themselves
--   exposed. In this case, we have 'RSList' to
--   represent a well-formed cons list, and 'RSDotted'
--   to represent an improper list of the form
--   @(a b c . d)@. This representation is based on
--   the shape of the parsed S-Expression, and not on
--   how it was represented, so @(a . (b))@ is going to
--   be represented as @RSList[RSAtom a, RSAtom b]@
--   despite having been originally represented as a
--   dotted list.
data RichSExpr atom
  = RSList [RichSExpr atom]
  | RSDotted [RichSExpr atom] atom
  | RSAtom atom
    deriving (Eq, Show, Read, Functor)

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
        go (SCons x xs) rs = go xs (rs . (toRich x:))

-- | This follows the same laws as 'toRich'.
fromRich :: RichSExpr atom -> SExpr atom
fromRich (RSAtom a) = SAtom a
fromRich (RSList xs) = foldr SCons SNil (map fromRich xs)
fromRich (RSDotted xs x) = foldr SCons (SAtom x) (map fromRich xs)

instance Foldable RichSExpr where
  foldMap f (RSAtom a)      = f a
  foldMap f (RSList xs)     = mconcat $ map (foldMap f) xs
  foldMap f (RSDotted xs y) = mconcat (map (foldMap f) xs) <> f y

instance Traversable RichSExpr where
  traverse f (RSAtom a)      = RSAtom <$> f a
  traverse f (RSList xs)     = RSList <$> sequenceA (map (traverse f) xs)
  traverse f (RSDotted xs y) = RSDotted <$> sequenceA (map (traverse f) xs)
                                        <*> f y

-- | A well-formed s-expression is one which does not
--   contain any dotted lists. This means that not
--   every value of @SExpr a@ can be converted to a
--   @WellFormedSExpr a@, although the opposite is
--   fine.
data WellFormedSExpr atom
  = WFSList [WellFormedSExpr atom]
  | WFSAtom atom
    deriving (Eq, Show, Read, Functor)

instance IsList (WellFormedSExpr atom) where
  type Item (WellFormedSExpr atom) = WellFormedSExpr atom
  fromList = WFSList
  toList (WFSList xs) = xs
  toList (WFSAtom {}) = error "Unable to turn atom into Haskell list"

instance IsString atom => IsString (WellFormedSExpr atom) where
  fromString = WFSAtom . fromString

instance Foldable WellFormedSExpr where
  foldMap f (WFSAtom a)  = f a
  foldMap f (WFSList xs) = mconcat $ map (foldMap f) xs

instance Traversable WellFormedSExpr where
  traverse f (WFSAtom a)  = WFSAtom <$> f a
  traverse f (WFSList xs) = WFSList <$> sequenceA (map (traverse f) xs)

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
  where go (SAtom a) rs = Left "Found atom in cdr position"
        go SNil rs      = return (WFSList (rs []))
        go (SCons x xs) rs = do
          x' <- toWellFormed x
          go xs (rs . (x':))

-- | Convert a WellFormedSExpr back into a SExpr.
fromWellFormed :: WellFormedSExpr atom -> SExpr atom
fromWellFormed (WFSAtom a)  = SAtom a
fromWellFormed (WFSList xs) =
  foldr SCons SNil (map fromWellFormed xs)
