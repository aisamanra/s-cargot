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
       , isNil
       , asAtom
       , asAssoc
       ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.SCargot.Repr as R

-- | A traversal with access to the first element of a pair.
--
-- >>> import Lens.Family
-- >>> set _car (A "elephant") (L [A "one", A "two", A "three"])
-- L [A "elelphant",A "two",A "three"]
-- >>> set _car (L [A "two", A "three"]) (DL [A "one"] "elephant")
-- DL [L[A "two",A "three"]] "elephant"
_car :: Applicative f => (RichSExpr a -> f (RichSExpr a)) -> RichSExpr a -> f (RichSExpr a)
_car f (L (x:xs))    = (\ x -> L (x:xs)) `fmap` f x
_car f (DL (x:xs) a) = (\ x -> DL (x:xs) a) `fmap` f x
_car _ (A a)         = pure (A a)
_car _ Nil           = pure Nil

-- | A traversal with access to the second element of a pair. Using
--   this to modify an s-expression may result in changing the
--   constructor used, changing a list to a dotted list or vice
--   versa.
--
-- >>> import Lens.Family
-- >>> set _cdr (A "elephant") (L [A "one", A "two", A "three"])
-- DL [A "one"] "elephant"
-- >>> set _cdr (L [A "two", A "three"]) (DL [A "one"] "elephant")
-- L [A "one",A "two",A "three"]
_cdr :: Applicative f => (RichSExpr a -> f (RichSExpr a)) -> RichSExpr a -> f (RichSExpr a)
_cdr f (L (x:xs)) =
  let go Nil     = L [x]
      go (A a)   = DL [x] a
      go (L xs') = L (x:xs')
  in go `fmap` f (L xs)
_cdr f (DL [x] a) =
  let go Nil    = L [x]
      go (A a') = DL [x] a'
      go (L xs) = L (x:xs)
  in go `fmap` f (A a)
_cdr f (DL (x:xs) a) =
  let go Nil    = L [x]
      go (A a') = DL [x] a'
      go (L xs) = L (x:xs)
  in go `fmap` f (DL xs a)
_cdr _ (A a)      = pure (A a)
_cdr _ Nil        = pure Nil

-- | A shorter infix alias to grab the head
--   and tail of an `RSList`.
pattern x ::: xs = R.RSList (x : xs)

-- | A shorter alias for `RSAtom`
pattern A a       = R.RSAtom a

-- | A shorter alias for `RSList`
pattern L xs      = R.RSList xs

-- | A shorter alias for `RSDotted`
pattern DL xs x = R.RSDotted xs x

-- | A shorter alias for `RSList` @[]@
pattern Nil = R.RSList []

-- | Utility function for parsing a pair of things: this parses a two-element list,
--   and not a cons pair.
--
-- >>> fromPair (isAtom "pachy") (asAtom return) (L [A "pachy", A "derm"])
-- Right ((), "derm")
-- >>> fromPair (isAtom "pachy") fromAtom (L [A "pachy"])
-- Left "Expected two-element list"
fromPair :: (RichSExpr t -> Either String a)
         -> (RichSExpr t -> Either String b)
         -> RichSExpr t -> Either String (a, b)
fromPair pl pr = asPair $ \(l,r) -> (,) <$> pl l <*> pr r

-- | Utility function for parsing a proper list of things.
--
-- >>> fromList fromAtom (L [A "this", A "that", A "the-other"])
-- Right ["this","that","the-other"]
-- >>> fromList fromAtom (DL [A "this", A "that"] "the-other"])
-- Left "asList: expected proper list; found dotted list"
fromList :: (RichSExpr t -> Either String a) -> RichSExpr t -> Either String [a]
fromList p = asList $ \ss -> mapM p ss

-- | Utility function for parsing a single atom
--
-- >>> fromAtom (A "elephant")
-- Right "elephant"
-- >>> fromAtom (L [A "elephant"])
-- Left "fromAtom: expected atom; found list"
fromAtom :: RichSExpr t -> Either String t
fromAtom (L _)    = Left "fromAtom: expected atom; found list"
fromAtom (DL _ _) = Left "fromAtom: expected atom; found dotted list"
fromAtom (A a)    = return a

-- | Parses a two-element list using the provided function.
--
-- >>> let go (A l) (A r) = return (l ++ r); go _ _ = Left "expected atoms"
-- >>> asPair go (L [A "pachy", A "derm"])
-- Right "pachyderm"
-- >>> asPair go (L [A "elephant"])
-- Left "asPair: expected two-element list; found list of length 1"
asPair :: ((RichSExpr t, RichSExpr t) -> Either String a)
       -> RichSExpr t -> Either String a
asPair f (L [l, r]) = f (l, r)
asPair _ (L ls)     = Left ("asPair: expected two-element list; found list of lenght " ++ show (length ls))
asPair _ DL {}      = Left ("asPair: expected two-element list; found dotted list")
asPair _ A {}       = Left ("asPair: expected two-element list; found atom")

-- | Parse an arbitrary-length list using the provided function.
--
-- >>> let go xs = concat <$> mapM fromAtom xs
-- >>> asList go (L [A "el", A "eph", A "ant"])
-- Right "elephant"
-- >>> asList go (DL [A "el", A "eph"] "ant")
-- Left "asList: expected list; found dotted list"
asList :: ([RichSExpr t] -> Either String a)
       -> RichSExpr t -> Either String a
asList f (L ls) = f ls
asList _ DL {}  = Left ("asList: expected list; found dotted list")
asList _ A { }  = Left ("asList: expected list; found dotted list")

-- | Match a given literal atom, failing otherwise.
--
-- >>> isAtom "elephant" (A "elephant")
-- Right ()
-- >>> isAtom "elephant" (L [A "elephant"])
-- Left "isAtom: expected atom; found list"
isAtom :: Eq t => t -> RichSExpr t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = Left "isAtom: failed to match atom"
isAtom _ L {}  = Left "isAtom: expected atom; found list"
isAtom _ DL {} = Left "isAtom: expected atom; found dotted list"

-- | Match an empty list, failing otherwise.
--
-- >>> isNil (L [])
-- Right ()
-- >>> isNil (A "elephant")
-- Left "isNil: expected nil; found atom"
isNil :: RichSExpr t -> Either String ()
isNil Nil   = return ()
isNil L {}  = Left "isNil: expected nil; found non-nil list"
isNil DL {} = Left "isNil: expected nil; found dotted list"
isNil A {}  = Left "isNil: expected nil; found atom"

-- | Parse an atom using the provided function.
--
-- >>> import Data.Char (toUpper)
-- >>> asAtom (return . map toUpper) (A "elephant")
-- Right "ELEPHANT"
-- >>> asAtom (return . map toUpper) (L [])
-- Left "asAtom: expected atom; found list"
asAtom :: (t -> Either String a) -> RichSExpr t -> Either String a
asAtom f (A s) = f s
asAtom _ L {}  = Left ("asAtom: expected atom; found list")
asAtom _ DL {} = Left ("asAtom: expected atom; found dotted list")

-- | Parse an assoc-list using the provided function.
--
-- >>> let def (x, y) = do { a <- fromAtom x; b <- fromAtom y; return (a ++ ": " ++ b) }
-- >>> let defList xs = do { defs <- mapM def xs; return (unlines defs) }
-- >>> asAssoc defList (L [ L [A "legs", A "four"], L [ A "trunk", A "one"] ])
-- Right "legs: four\ntrunk: one\n"
-- >>> asAssoc defList (L [ L [A "legs", A "four"], L [ A "elephant"] ])
-- Left "asAssoc: expected pair; found list of length 1"
asAssoc :: ([(RichSExpr t, RichSExpr t)] -> Either String a)
        -> RichSExpr t -> Either String a
asAssoc f (L ss) = gatherPairs ss >>= f
  where gatherPairs (L [a, b] : ss) = (:) <$> pure (a, b) <*> gatherPairs ss
        gatherPairs []              = pure []
        gatherPairs (A {} : _)      = Left ("asAssoc: expected pair; found atom")
        gatherPairs (DL {} : _)     = Left ("asAssoc: expected pair; found dotted list")
        gatherPairs (L ls : _)      = Left ("asAssoc: expected pair; found list of length " ++ show (length ls))
asAssoc f DL {}     = Left "asAssoc: expected assoc list; found dotted list"
asAssoc f A {}      = Left "asAssoc: expected assoc list; found atom"

car :: (RichSExpr t -> Either String t') -> [RichSExpr t] -> Either String t'
car f (x:_) = f x
car _ []    = Left "car: Taking car of zero-element list"

cdr :: ([RichSExpr t] -> Either String t') -> [RichSExpr t] -> Either String t'
cdr f (_:xs) = f xs
cdr _ []     = Left "cdr: Taking cdr of zero-element list"
