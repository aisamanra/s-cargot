{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SCargot.Repr.Basic
       ( -- * Basic 'SExpr' representation
         R.SExpr(..)
         -- * Constructing and Deconstructing
       , cons
       , uncons
         -- * Shorthand Patterns
       , pattern (:::)
       , pattern A
       , pattern L
       , pattern DL
       , pattern Nil
         -- * Lenses
       , _car
       , _cdr
         -- * Useful processing functions
       , fromPair
       , fromList
       , fromAtom
       , asPair
       , asList
       , isAtom
       , asAtom
       , asAssoc
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), (<*>), pure)
#endif
import Data.SCargot.Repr as R

-- | A traversal with access to the first element of a pair.
--
-- >>> import Lens.Family
-- >>> set _car (A "elephant") (A "one" ::: A "two" ::: A "three" ::: Nil)
-- A "elelphant" ::: A "two" ::: A "three" ::: Nil
-- >>> set _car (A "two" ::: A "three" ::: Nil) (A "one" ::: A "elephant")
-- (A "two" ::: A "three" ::: Nil) ::: A "elephant"
_car :: Applicative f => (SExpr a -> f (SExpr a)) -> SExpr a -> f (SExpr a)
_car f (SCons x xs) = (:::) <$> f x <*> pure xs
_car _ (SAtom a)    = pure (A a)
_car _ SNil         = pure SNil

-- | A traversal with access to the second element of a pair.
--
-- >>> import Lens.Family
-- >>> set _cdr (A "elephant") (A "one" ::: A "two" ::: A "three" ::: Nil)
-- A "one" ::: A "elephant"
-- >>> set _cdr (A "two" ::: A "three" ::: Nil) (A "one" ::: A "elephant")
-- A "one" ::: A "two" ::: A "three" ::: Nil
_cdr :: Applicative f => (SExpr a -> f (SExpr a)) -> SExpr a -> f (SExpr a)
_cdr f (SCons x xs) = (:::) <$> pure x <*> f xs
_cdr _ (SAtom a)    = pure (A a)
_cdr _ SNil         = pure Nil

-- | Produce the head and tail of the s-expression (if possible).
--
-- >>> uncons (A "el" ::: A "eph" ::: A "ant" ::: Nil)
-- Just (A "el",SCons (SAtom "eph") (SCons (SAtom "ant") SNil))
uncons :: SExpr a -> Maybe (SExpr a, SExpr a)
uncons (SCons x xs) = Just (x, xs)
uncons _            = Nothing

-- | Combine the two s-expressions into a new one.
--
-- >>> cons (A "el") (L ["eph", A "ant"])
-- SCons (SAtom "el) (SCons (SAtom "eph") (SCons (SAtom "ant") SNil))
cons :: SExpr a -> SExpr a -> SExpr a
cons = SCons

gatherDList :: SExpr a -> Maybe ([SExpr a], a)
gatherDList SNil     = Nothing
gatherDList SAtom {} = Nothing
gatherDList sx       = go sx
  where go SNil = Nothing
        go (SAtom a) = return ([], a)
        go (SCons x xs) = do
          (ys, a) <- go xs
          return (x:ys, a)

infixr 5 :::

-- | A shorter infix alias for `SCons`
--
-- >>> A "pachy" ::: A "derm"
-- SCons (SAtom "pachy") (SAtom "derm")
pattern (:::) :: SExpr a -> SExpr a -> SExpr a
pattern x ::: xs = SCons x xs

-- | A shorter alias for `SAtom`
--
-- >>> A "elephant"
-- SAtom "elephant"
pattern A :: a -> SExpr a
pattern A x = SAtom x

-- | A (slightly) shorter alias for `SNil`
--
-- >>> Nil
-- SNil
pattern Nil :: SExpr a
pattern Nil = SNil

-- | An alias for matching a proper list.
--
-- >>> L [A "pachy", A "derm"]
-- SExpr (SAtom "pachy") (SExpr (SAtom "derm") SNil)
pattern L :: [SExpr a] -> SExpr a
pattern L xs <- (gatherList -> Right xs)
#if MIN_VERSION_base(4,8,0)
  where L []     = SNil
        L (x:xs) = SCons x (L xs)
#endif


-- | An alias for matching a dotted list.
--
-- >>> DL [A "pachy"] A "derm"
-- SExpr (SAtom "pachy") (SAtom "derm")
pattern DL :: [SExpr a] -> a -> SExpr a
pattern DL xs x <- (gatherDList -> Just (xs, x))
#if MIN_VERSION_base(4,8,0)
  where DL []     a = SAtom a
        DL (x:xs) a = SCons x (DL xs a)
#endif

getShape :: SExpr a -> String
getShape Nil = "empty list"
getShape sx = go (0 :: Int) sx
  where go n SNil         = "list of length " ++ show n
        go n SAtom {}     = "dotted list of length " ++ show n
        go n (SCons _ xs) = go (n+1) xs

-- | Utility function for parsing a pair of things.
--
-- >>> fromPair (isAtom "pachy") (asAtom return) (A "pachy" ::: A "derm" ::: Nil)
-- Right ((), "derm")
-- >>> fromPair (isAtom "pachy") fromAtom (A "pachy" ::: Nil)
-- Left "Expected two-element list"
fromPair :: (SExpr t -> Either String a)
         -> (SExpr t -> Either String b)
         -> SExpr t -> Either String (a, b)
fromPair pl pr (l ::: r ::: Nil) = (,) <$> pl l <*> pr r
fromPair _  _  sx = Left ("fromPair: expected two-element list; found " ++ getShape sx)

-- | Utility function for parsing a list of things.
fromList :: (SExpr t -> Either String a) -> SExpr t -> Either String [a]
fromList p (s ::: ss) = (:) <$> p s <*> fromList p ss
fromList _ Nil        = pure []
fromList _ sx         = Left ("fromList: expected list; found " ++ getShape sx)

-- | Utility function for parsing a single atom
fromAtom :: SExpr t -> Either String t
fromAtom (A a) = return a
fromAtom sx    = Left ("fromAtom: expected atom; found list" ++ getShape sx)

gatherList :: SExpr t -> Either String [SExpr t]
gatherList (x ::: xs) = (:) <$> pure x <*> gatherList xs
gatherList Nil        = pure []
gatherList sx         = Left ("gatherList: expected list; found " ++ getShape sx)

-- | Parse a two-element list (NOT a dotted pair) using the
--   provided function.
--
-- >>> let go (A l) (A r) = return (l ++ r); go _ _ = Left "expected atoms"
-- >>> asPair go (A "pachy" ::: A "derm" ::: Nil)
-- Right "pachyderm"
-- >>> asPair go (A "elephant" ::: Nil)
-- Left "asPair: expected two-element list; found list of length 1"
asPair :: ((SExpr t, SExpr t) -> Either String a)
       -> SExpr t -> Either String a
asPair f (l ::: r ::: SNil) = f (l, r)
asPair _ sx = Left ("asPair: expected two-element list; found " ++ getShape sx)

-- | Parse an arbitrary-length list using the provided function.
--
-- >>> let go xs = concat <$> mapM fromAtom xs
-- >>> asList go (A "el" ::: A "eph" ::: A "ant" ::: Nil)
-- Right "elephant"
-- >>> asList go (A "el" ::: A "eph" ::: A "ant")
-- Left "asList: expected list; found dotted list of length 3"
asList :: ([SExpr t] -> Either String a) -> SExpr t -> Either String a
asList f ls = gatherList ls >>= f

-- | Match a given literal atom, failing otherwise.
--
-- >>> isAtom "elephant" (A "elephant")
-- Right ()
-- >>> isAtom "elephant" (A "elephant" ::: Nil)
-- Left "isAtom: expected atom; found list"
isAtom :: Eq t => t -> SExpr t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = Left "isAtom: failed to match atom"
isAtom _ sx = Left ("isAtom: expected atom; found " ++ getShape sx)

-- | Parse an atom using the provided function.
--
-- >>> import Data.Char (toUpper)
-- >>> asAtom (return . map toUpper) (A "elephant")
-- Right "ELEPHANT"
-- >>> asAtom (return . map toUpper) Nil
-- Left "asAtom: expected atom; found empty list"
asAtom :: (t -> Either String a) -> SExpr t -> Either String a
asAtom f (A s) = f s
asAtom _ sx    = Left ("asAtom: expected atom; found " ++ getShape sx)

-- | Parse an assoc-list using the provided function.
--
-- >>> let def (x, y) = do { a <- fromAtom x; b <- fromAtom y; return (a ++ ": " ++ b) }
-- >>> let defList xs = do { defs <- mapM def xs; return (unlines defs) }
-- >>> asAssoc defList ((A "legs" ::: A "four" ::: Nil) ::: (A "trunk" ::: A "one" ::: Nil) ::: Nil)
-- Right "legs: four\ntrunk: one\n"
-- >>> asAssoc defList ((A "legs" ::: A "four" ::: Nil) ::: (A "elephant") ::: Nil)
-- Left "asAssoc: expected pair; found list of length 1"
asAssoc :: ([(SExpr t, SExpr t)] -> Either String a)
        -> SExpr t -> Either String a
asAssoc f ss = gatherList ss >>= mapM go >>= f
  where go (a ::: b ::: Nil) = return (a, b)
        go sx = Left ("asAssoc: expected pair; found " ++ getShape sx)
