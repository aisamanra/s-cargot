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

import Control.Applicative ((<$>), (<*>), pure)
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

-- | Utility function for parsing a pair of things.
fromPair :: (RichSExpr t -> Either String a)
         -> (RichSExpr t -> Either String b)
         -> RichSExpr t -> Either String (a, b)
fromPair pl pr = asPair $ \(l,r) -> (,) <$> pl l <*> pr r

-- | Utility function for parsing a list of things.
fromList :: (RichSExpr t -> Either String a) -> RichSExpr t -> Either String [a]
fromList p = asList $ \ss -> mapM p ss

-- | Utility function for parsing a single atom
fromAtom :: RichSExpr t -> Either String t
fromAtom (L _) = Left "Expected atom; found list"
fromAtom (A a) = return a

-- | RichSExpr a -> Either String two-element list (NOT a dotted pair) using the
--   provided function.
asPair :: ((RichSExpr t, RichSExpr t) -> Either String a)
       -> RichSExpr t -> Either String a
asPair f (L [l, r]) = f (l, r)
asPair _ sx         = Left ("Expected two-element list")

-- | Parse an arbitrary-length list using the provided function.
asList :: ([RichSExpr t] -> Either String a)
       -> RichSExpr t -> Either String a
asList f (L ls) = f ls
asList _ sx     = Left ("Expected list")

-- | Match a given literal atom, failing otherwise.
isAtom :: Eq t => t -> RichSExpr t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = Left ".."
isAtom _ _  = Left ".."

-- | Parse an atom using the provided function.
asAtom :: (t -> Either String a) -> RichSExpr t -> Either String a
asAtom f (A s) = f s
asAtom _ sx    = Left ("Expected atom; got list")

-- | Parse an assoc-list using the provided function.
asAssoc :: Show t => ([(RichSExpr t, RichSExpr t)] -> Either String a)
        -> RichSExpr t -> Either String a
asAssoc f (L ss) = gatherPairs ss >>= f
  where gatherPairs (L [a, b] : ss) = (:) <$> pure (a, b) <*> gatherPairs ss
        gatherPairs []              = pure []
        gatherPairs _               = Left "..."
asAssoc _ sx     = Left ("Expected assoc list; got " ++ show sx)

car :: (RichSExpr t -> Either String t') -> [RichSExpr t] -> Either String t'
car f (x:_) = f x
car _ []    = Left "car: Taking car of zero-element list"

cdr :: ([RichSExpr t] -> Either String t') -> [RichSExpr t] -> Either String t'
cdr f (_:xs) = f xs
cdr _ []     = Left "cdr: Taking cdr of zero-element list"
