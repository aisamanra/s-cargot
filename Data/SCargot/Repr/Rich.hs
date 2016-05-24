{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SCargot.Repr.Rich
       ( -- * 'RichSExpr' representation
         R.RichSExpr(..)
       , R.toRich
       , R.fromRich
         -- * Constructing and Deconstructing
       , cons
       , uncons
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
       , car
       , cdr
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), (<*>), pure)
#endif
import Data.SCargot.Repr as R

-- | A traversal with access to the first element of a pair.
--
-- >>> import Lens.Family
-- >>> set _car (A "elephant") (L [A "one", A "two", A "three"])
-- L [A "elelphant",A "two",A "three"]
-- >>> set _car (L [A "two", A "three"]) (DL [A "one"] "elephant")
-- DL [L[A "two",A "three"]] "elephant"
_car :: Applicative f => (RichSExpr a -> f (RichSExpr a)) -> RichSExpr a -> f (RichSExpr a)
_car f (RSList (x:xs))     = (\ y -> L (y:xs)) `fmap` f x
_car f (RSDotted (x:xs) a) = (\ y -> DL (y:xs) a) `fmap` f x
_car _ (RSAtom a)          = pure (A a)
_car _ (RSList [])         = pure Nil
_car _ (RSDotted [] a)     = pure (A a)

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
_cdr f (RSList (x:xs)) =
  let go (RSList [])      = L [x]
      go (RSAtom a)       = DL [x] a
      go (RSList xs')     = L (x:xs')
      go (RSDotted ys a') = DL (x:ys) a'
  in go `fmap` f (L xs)
_cdr f (RSDotted [x] a) =
  let go (RSList [])      = L [x]
      go (RSAtom a')      = DL [x] a'
      go (RSList xs)      = L (x:xs)
      go (RSDotted ys a') = DL (x:ys) a'
  in go `fmap` f (A a)
_cdr f (RSDotted (x:xs) a) =
  let go (RSList [])      = L [x]
      go (RSAtom a')      = DL [x] a'
      go (RSList ys)      = L (x:ys)
      go (RSDotted ys a') = DL (x:ys) a'
  in go `fmap` f (DL xs a)
_cdr _ (RSAtom a)         = pure (A a)
_cdr _ (RSList [])        = pure Nil
_cdr _ (RSDotted [] a)    = pure (A a)

-- | Produce the head and tail of the s-expression (if possible).
--
-- >>> uncons (L [A "el", A "eph", A "ant"])
-- Just (A "el",L [A "eph",A "ant"])
uncons :: RichSExpr a -> Maybe (RichSExpr a, RichSExpr a)
uncons (R.RSList (x:xs))     = Just (x, R.RSList xs)
uncons (R.RSDotted (x:xs) a) = Just (x, R.RSDotted xs a)
uncons _                     = Nothing

-- | Combine the two s-expressions into a new one.
--
-- >>> cons (A "el") (L [A "eph", A "ant"])
-- L [A "el",A "eph",A "ant"]
cons :: RichSExpr a -> RichSExpr a -> RichSExpr a
cons x (R.RSList xs)     = R.RSList (x:xs)
cons x (R.RSDotted xs a) = R.RSDotted (x:xs) a
cons x (R.RSAtom a)      = R.RSDotted [x] a

-- | A shorter infix alias to grab the head
--   and tail of an `RSList`.
--
-- >>> A "one" ::: L [A "two", A "three"]
-- RSList [RSAtom "one",RSAtom "two",RSAtom "three"]
pattern (:::) :: RichSExpr a -> RichSExpr a -> RichSExpr a
pattern x ::: xs <- (uncons -> Just (x, xs))
#if MIN_VERSION_base(4,8,0)
  where x ::: xs = cons x xs
#endif

-- | A shorter alias for `RSAtom`
--
-- >>> A "elephant"
-- RSAtom "elephant"
pattern A :: a -> RichSExpr a
pattern A a = R.RSAtom a

-- | A shorter alias for `RSList`
--
-- >>> L [A "pachy", A "derm"]
-- RSList [RSAtom "pachy",RSAtom "derm"]
pattern L :: [RichSExpr a] -> RichSExpr a
pattern L xs = R.RSList xs

-- | A shorter alias for `RSDotted`
--
-- >>> DL [A "pachy"] "derm"
-- RSDotted [RSAtom "pachy"] "derm"
pattern DL :: [RichSExpr a] -> a -> RichSExpr a
pattern DL xs x = R.RSDotted xs x

-- | A shorter alias for `RSList` @[]@
--
-- >>> Nil
-- RSList []
pattern Nil :: RichSExpr a
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
fromAtom (RSList _)     = Left "fromAtom: expected atom; found list"
fromAtom (RSDotted _ _) = Left "fromAtom: expected atom; found dotted list"
fromAtom (RSAtom a)     = return a

-- | Parses a two-element list using the provided function.
--
-- >>> let go (A l) (A r) = return (l ++ r); go _ _ = Left "expected atoms"
-- >>> asPair go (L [A "pachy", A "derm"])
-- Right "pachyderm"
-- >>> asPair go (L [A "elephant"])
-- Left "asPair: expected two-element list; found list of length 1"
asPair :: ((RichSExpr t, RichSExpr t) -> Either String a)
       -> RichSExpr t -> Either String a
asPair f (RSList [l, r]) = f (l, r)
asPair _ (RSList ls)     = Left ("asPair: expected two-element list; found list of lenght " ++ show (length ls))
asPair _ RSDotted {}     = Left ("asPair: expected two-element list; found dotted list")
asPair _ RSAtom {}       = Left ("asPair: expected two-element list; found atom")

-- | Parse an arbitrary-length list using the provided function.
--
-- >>> let go xs = concat <$> mapM fromAtom xs
-- >>> asList go (L [A "el", A "eph", A "ant"])
-- Right "elephant"
-- >>> asList go (DL [A "el", A "eph"] "ant")
-- Left "asList: expected list; found dotted list"
asList :: ([RichSExpr t] -> Either String a)
       -> RichSExpr t -> Either String a
asList f (RSList ls) = f ls
asList _ RSDotted {} = Left ("asList: expected list; found dotted list")
asList _ RSAtom { }  = Left ("asList: expected list; found dotted list")

-- | Match a given literal atom, failing otherwise.
--
-- >>> isAtom "elephant" (A "elephant")
-- Right ()
-- >>> isAtom "elephant" (L [A "elephant"])
-- Left "isAtom: expected atom; found list"
isAtom :: Eq t => t -> RichSExpr t -> Either String ()
isAtom s (RSAtom s')
  | s == s'   = return ()
  | otherwise = Left "isAtom: failed to match atom"
isAtom _ RSList {}  = Left "isAtom: expected atom; found list"
isAtom _ RSDotted {} = Left "isAtom: expected atom; found dotted list"

-- | Match an empty list, failing otherwise.
--
-- >>> isNil (L [])
-- Right ()
-- >>> isNil (A "elephant")
-- Left "isNil: expected nil; found atom"
isNil :: RichSExpr t -> Either String ()
isNil (RSList []) = return ()
isNil RSList {}   = Left "isNil: expected nil; found non-nil list"
isNil RSDotted {} = Left "isNil: expected nil; found dotted list"
isNil RSAtom {}   = Left "isNil: expected nil; found atom"

-- | Parse an atom using the provided function.
--
-- >>> import Data.Char (toUpper)
-- >>> asAtom (return . map toUpper) (A "elephant")
-- Right "ELEPHANT"
-- >>> asAtom (return . map toUpper) (L [])
-- Left "asAtom: expected atom; found list"
asAtom :: (t -> Either String a) -> RichSExpr t -> Either String a
asAtom f (RSAtom s)  = f s
asAtom _ RSList {}   = Left ("asAtom: expected atom; found list")
asAtom _ RSDotted {} = Left ("asAtom: expected atom; found dotted list")

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
asAssoc f (RSList ss) = gatherPairs ss >>= f
  where gatherPairs (RSList [a, b] : ts) = (:) <$> pure (a, b) <*> gatherPairs ts
        gatherPairs []              = pure []
        gatherPairs (RSAtom {} : _)      = Left ("asAssoc: expected pair; found atom")
        gatherPairs (RSDotted {} : _)     = Left ("asAssoc: expected pair; found dotted list")
        gatherPairs (RSList ls : _)      = Left ("asAssoc: expected pair; found list of length " ++ show (length ls))
asAssoc _ RSDotted {} = Left "asAssoc: expected assoc list; found dotted list"
asAssoc _ RSAtom {}   = Left "asAssoc: expected assoc list; found atom"

car :: (RichSExpr t -> Either String t') -> [RichSExpr t] -> Either String t'
car f (x:_) = f x
car _ []    = Left "car: Taking car of zero-element list"

cdr :: ([RichSExpr t] -> Either String t') -> [RichSExpr t] -> Either String t'
cdr f (_:xs) = f xs
cdr _ []     = Left "cdr: Taking cdr of zero-element list"
