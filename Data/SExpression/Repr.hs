module Data.SExpression.Repr
       ( SExpr(..)
       , RichSExpr(..)
       , toRich
       , fromRich
       , WellFormedSExpr(..)
       , toWellFormed
       , fromWellFormed
       ) where

-- | All S-Expressions can be understood as a sequence
--   of @cons@ cells (represented here by 'SCons'), the
--   empty list @nil@ (represented by 'SNil') or an
--   @atom@.
data SExpr atom
  = SCons (SExpr atom) (SExpr atom)
  | SAtom atom
  | SNil
    deriving (Eq, Show, Read)

-- | Sometimes, the cons-based interface is too low
--   level, and we'd rather have the lists themselves
--   exposed. In this case, we have 'RSList' to
--   represent a well-formed cons list, and 'RSDotted'
--   to represent an improper list of the form
--   @(a b c . d)@.
data RichSExpr atom
  = RSList [RichSExpr atom]
  | RSDotted [RichSExpr atom] atom
  | RSAtom atom
    deriving (Eq, Show, Read)

-- | A Rich S-Expression might be a nicer interface
--   for certain libraries. It should always be true
--   that
--
--   > fromRich . toRich == id
--
--   and that
--
--   > toRich . fromRich == id
toRich :: SExpr atom -> RichSExpr atom
toRich (SAtom a) = RSAtom a
toRich (SCons x xs) = go xs [toRich x]
  where go (SAtom a) rs    = RSDotted rs a
        go SNil rs         = RSList rs
        go (SCons x xs) rs = go xs (toRich x:rs)

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
    deriving (Eq, Show, Read)

-- | This will be @Nothing@ is the argument contains an
--   improper list. It should hold that
--
--   > toWellFormed . fromWellFormed == Right
toWellFormed :: SExpr atom -> Either String (WellFormedSExpr atom)
toWellFormed (SAtom a) = return (WFSAtom a)
toWellFormed (SCons x xs) = do
  x' <- toWellFormed x
  go xs [x']
  where go (SAtom a) rs = Left "Found atom in cdr position"
        go SNil rs      = return (WFSList rs)
        go (SCons x xs) rs = do
          x' <- toWellFormed x
          go xs (x':rs)

-- | Convert a WellFormedSExpr back into a SExpr.
fromWellFormed :: WellFormedSExpr atom -> SExpr atom
fromWellFormed (WFSAtom a)  = SAtom a
fromWellFormed (WFSList xs) =
  foldr SCons SNil (map fromWellFormed xs)
