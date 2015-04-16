{-# LANGUAGE PatternSynonyms #-}

module Data.SCargot.Repr.WellFormed
       ( -- * 'WellFormedSExpr' representation
         R.WellFormedSExpr(..)
       , R.toWellFormed
       , R.fromWellFormed
         -- * Useful pattern synonyms
       , pattern (:::)
       , pattern L
       , pattern A
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
       , car
       , cdr
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.SCargot.Repr as R

-- | A shorter infix alias to grab the head
--   and tail of a `WFSList`
pattern x ::: xs = R.WFSList (x : xs)

-- | A shorter alias for `WFSList`
pattern L xs = R.WFSList xs

-- | A shorter alias for `WFSAtom`
pattern A a  = R.WFSAtom a

-- | A shorter alias for `WFSList []`
pattern Nil = R.WFSList []

type S t = R.WellFormedSExpr t
type Parse t a = R.WellFormedSExpr t -> Either String a

-- | Utility function for parsing a pair of things.
fromPair :: (WellFormedSExpr t -> Either String a)
         -> (WellFormedSExpr t -> Either String b)
         -> WellFormedSExpr t -> Either String (a, b)
fromPair pl pr (L [l, r]) = (,) <$> pl l <*> pr r
fromPair _  _  sx = Left ("Expected two-element list")

-- | Utility function for parsing a list of things.
fromList :: (WellFormedSExpr t -> Either String a)
         -> WellFormedSExpr t -> Either String [a]
fromList p (L ss) = mapM p ss
fromList _ sx     = Left ("Expected list")

fromAtom :: WellFormedSExpr t -> Either String t
fromAtom (L _) = Left "Expected atom; found list"
fromAtom (A a) = return a

asPair :: ((WellFormedSExpr t, WellFormedSExpr t) -> Either String a)
       -> WellFormedSExpr t -> Either String a
asPair f (L [l, r]) = f (l, r)
asPair _ sx         = Left ("Expected two-element list")

asList :: ([WellFormedSExpr t] -> Either String a)
       -> WellFormedSExpr t -> Either String a
asList f (L ls) = f ls
asList _ sx     = Left ("Expected list")

isAtom :: Eq t => t -> WellFormedSExpr t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = Left ".."
isAtom _ _  = Left ".."

asAtom :: (t -> Either String a) -> WellFormedSExpr t -> Either String a
asAtom f (A s) = f s
asAtom _ sx    = Left ("Expected atom; got list")

asAssoc :: ([(WellFormedSExpr t, WellFormedSExpr t)] -> Either String a)
        -> WellFormedSExpr t -> Either String a
asAssoc f (L ss) = gatherPairs ss >>= f
  where gatherPairs (L [a, b] : ss) = (:) <$> pure (a, b) <*> gatherPairs ss
        gatherPairs []              = pure []
        gatherPairs _               = Left "..."
asAssoc _ sx     = Left ("Expected assoc list")

car :: (WellFormedSExpr t -> Either String t')
    -> [WellFormedSExpr t] -> Either String t'
car f (x:_) = f x
car _ []    = Left "car: Taking car of zero-element list"

cdr :: ([WellFormedSExpr t] -> Either String t')
    -> [WellFormedSExpr t] -> Either String t'
cdr f (_:xs) = f xs
cdr _ []     = Left "cdr: Taking cdr of zero-element list"
