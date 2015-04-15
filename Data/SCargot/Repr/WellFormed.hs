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
fromPair :: Parse t a -> Parse t b -> Parse t (a, b)
fromPair pl pr (L [l, r]) = (,) <$> pl l <*> pr r
fromPair _  _  sx = fail ("Expected two-element list")

-- | Utility function for parsing a list of things.
fromList :: Parse t a -> Parse t [a]
fromList p (L ss) = mapM p ss
fromList _ sx     = fail ("Expected list")

fromAtom :: Parse t t
fromAtom (L _) = fail "Expected atom; found list"
fromAtom (A a) = return a

asPair :: ((S t, S t) -> Either String a) -> S t -> Either String a
asPair f (L [l, r]) = f (l, r)
asPair _ sx         = fail ("Expected two-element list")

asList :: ([S t] -> Either String a) -> S t -> Either String a
asList f (L ls) = f ls
asList _ sx     = fail ("Expected list")

isAtom :: Eq t => t -> S t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = fail ".."
isAtom _ _  = fail ".."

asAtom :: Show t => (t -> Either String a) -> S t -> Either String a
asAtom f (A s) = f s
asAtom _ sx    = fail ("Expected atom; got" ++ show sx)

asAssoc :: Show t => ([(S t, S t)] -> Either String a) -> S t -> Either String a
asAssoc f (L ss) = gatherPairs ss >>= f
  where gatherPairs (L [a, b] : ss) = (:) <$> pure (a, b) <*> gatherPairs ss
        gatherPairs []              = pure []
        gatherPairs _               = fail "..."
asAssoc _ sx     = fail ("Expected assoc list; got " ++ show sx)

car :: (S t -> Either String t') -> [S t] -> Either String t'
car f (x:_) = f x
car _ []    = fail "car: Taking car of zero-element list"

cdr :: ([S t] -> Either String t') -> [S t] -> Either String t'
cdr f (_:xs) = f xs
cdr _ []     = fail "cdr: Taking cdr of zero-element list"
