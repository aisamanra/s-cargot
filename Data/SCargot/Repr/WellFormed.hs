{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SCargot.Repr.WellFormed
       ( -- * 'WellFormedSExpr' representation
         R.WellFormedSExpr(..)
       , R.toWellFormed
       , R.fromWellFormed
         -- * Constructing and Deconstructing
       , cons
       , uncons
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
       , isNil
       , asAtom
       , asAssoc
       , car
       , cdr
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Data.SCargot.Repr as R

-- | Produce the head and tail of the s-expression (if possible).
--
-- >>> uncons (L [A "el", A "eph", A "ant"])
-- Just (WFSAtom "el",WFSList [WFSAtom "eph",WFSAtom "ant"])
uncons :: WellFormedSExpr a -> Maybe (WellFormedSExpr a, WellFormedSExpr a)
uncons R.WFSAtom {}       = Nothing
uncons (R.WFSList [])     = Nothing
uncons (R.WFSList (x:xs)) = Just (x, R.WFSList xs)

-- | Combine the two-expressions into a new one. This will return
--   @Nothing@ if the resulting s-expression is not well-formed.
--
-- >>> cons (A "el") (L [A "eph", A "ant"])
-- Just (WFSList [WFSAtom "el",WFSAtom "eph",WFSAtom "ant"])
-- >>> cons (A "pachy") (A "derm"))
-- Nothing
cons :: WellFormedSExpr a -> WellFormedSExpr a -> Maybe (WellFormedSExpr a)
cons _ (R.WFSAtom {}) = Nothing
cons x (R.WFSList xs) = Just (R.WFSList (x:xs))

-- | A shorter infix alias to grab the head and tail of a `WFSList`. This
--   pattern is unidirectional, because it cannot be guaranteed that it
--   is used to construct well-formed s-expressions; use the function "cons"
--   instead.
--
-- >>> let sum (x ::: xs) = x + sum xs; sum Nil = 0
pattern x ::: xs <- (uncons -> Just (x, xs))

-- | A shorter alias for `WFSList`
--
-- >>> L [A "pachy", A "derm"]
-- WFSList [WFSAtom "pachy",WFSAtom "derm"]
pattern L xs = R.WFSList xs

-- | A shorter alias for `WFSAtom`
--
-- >>> A "elephant"
-- WFSAtom "elephant"
pattern A a  = R.WFSAtom a

-- | A shorter alias for `WFSList` @[]@
--
-- >>> Nil
-- WFSList []
pattern Nil = R.WFSList []

getShape :: WellFormedSExpr a -> String
getShape WFSAtom {}   = "atom"
getShape (WFSList []) = "empty list"
getShape (WFSList sx) = "list of length " ++ show (length sx)

-- | Utility function for parsing a pair of things.
--
-- >>> fromPair (isAtom "pachy") (asAtom return) (L [A "pachy", A "derm"])
-- Right ((), "derm")
-- >>> fromPair (isAtom "pachy") fromAtom (L [A "pachy"])
-- Left "Expected two-element list"
fromPair :: (WellFormedSExpr t -> Either String a)
         -> (WellFormedSExpr t -> Either String b)
         -> WellFormedSExpr t -> Either String (a, b)
fromPair pl pr (L [l, r]) = (,) <$> pl l <*> pr r
fromPair _  _  sx = Left ("fromPair: expected two-element list; found " ++ getShape sx)

-- | Utility function for parsing a list of things.
--
-- >>> fromList fromAtom (L [A "this", A "that", A "the-other"])
-- Right ["this","that","the-other"]
-- >>> fromList fromAtom (A "pachyderm")
-- Left "asList: expected proper list; found dotted list"
fromList :: (WellFormedSExpr t -> Either String a)
         -> WellFormedSExpr t -> Either String [a]
fromList p (L ss) = mapM p ss
fromList _ sx     = Left ("fromList: expected list; found " ++ getShape sx)

-- | Utility function for parsing a single atom
--
-- >>> fromAtom (A "elephant")
-- Right "elephant"
-- >>> fromAtom (L [A "elephant"])
-- Left "fromAtom: expected atom; found list"
fromAtom :: WellFormedSExpr t -> Either String t
fromAtom (A a) = return a
fromAtom sx    = Left ("fromAtom: expected atom; found " ++ getShape sx)

-- | Parses a two-element list using the provided function.
--
-- >>> let go (A l) (A r) = return (l ++ r); go _ _ = Left "expected atoms"
-- >>> asPair go (L [A "pachy", A "derm"])
-- Right "pachyderm"
-- >>> asPair go (L [A "elephant"])
-- Left "asPair: expected two-element list; found list of length 1"
asPair :: ((WellFormedSExpr t, WellFormedSExpr t) -> Either String a)
       -> WellFormedSExpr t -> Either String a
asPair f (L [l, r]) = f (l, r)
asPair _ sx         = Left ("asPair: expected two-element list; found " ++ getShape sx)

-- | Parse an arbitrary-length list using the provided function.
--
-- >>> let go xs = concat <$> mapM fromAtom xs
-- >>> asList go (L [A "el", A "eph", A "ant"])
-- Right "elephant"
-- >>> asList go (A "pachyderm")
-- Left "asList: expected list; found atom"
asList :: ([WellFormedSExpr t] -> Either String a)
       -> WellFormedSExpr t -> Either String a
asList f (L ls) = f ls
asList _ sx     = Left ("asList: expected list; found " ++ getShape sx)

-- | Match a given literal atom, failing otherwise.
--
-- >>> isAtom "elephant" (A "elephant")
-- Right ()
-- >>> isAtom "elephant" (L [A "elephant"])
-- Left "isAtom: expected atom; found list"
isAtom :: Eq t => t -> WellFormedSExpr t -> Either String ()
isAtom s (A s')
  | s == s'   = return ()
  | otherwise = Left "isAtom: failed to match atom"
isAtom _ sx  = Left ("isAtom: expected atom; found " ++ getShape sx)

-- | Match an empty list, failing otherwise.
--
-- >>> isNil (L [])
-- Right ()
-- >>> isNil (A "elephant")
-- Left "isNil: expected nil; found atom"
isNil :: WellFormedSExpr t -> Either String ()
isNil Nil = return ()
isNil sx  = Left ("isNil: expected nil; found " ++ getShape sx)

-- | Parse an atom using the provided function.
--
-- >>> import Data.Char (toUpper)
-- >>> asAtom (return . map toUpper) (A "elephant")
-- Right "ELEPHANT"
-- >>> asAtom (return . map toUpper) (L [])
-- Left "asAtom: expected atom; found list"
asAtom :: (t -> Either String a) -> WellFormedSExpr t -> Either String a
asAtom f (A s) = f s
asAtom _ sx    = Left ("asAtom: expected atom; found " ++ getShape sx)

-- | Parse an assoc-list using the provided function.
--
-- >>> let def (x, y) = do { a <- fromAtom x; b <- fromAtom y; return (a ++ ": " ++ b) }
-- >>> let defList xs = do { defs <- mapM def xs; return (unlines defs) }
-- >>> asAssoc defList (L [ L [A "legs", A "four"], L [ A "trunk", A "one"] ])
-- Right "legs: four\ntrunk: one\n"
-- >>> asAssoc defList (L [ L [A "legs", A "four"], L [ A "elephant"] ])
-- Left "asAssoc: expected pair; found list of length 1"
asAssoc :: ([(WellFormedSExpr t, WellFormedSExpr t)] -> Either String a)
        -> WellFormedSExpr t -> Either String a
asAssoc f (L ss) = gatherPairs ss >>= f
  where gatherPairs (L [a, b] : ts) = (:) <$> pure (a, b) <*> gatherPairs ts
        gatherPairs []              = pure []
        gatherPairs (sx:_)          = Left ("asAssoc: expected pair; found " ++ getShape sx)
asAssoc _ sx     = Left ("asAssoc: expected list; found " ++ getShape sx)

-- | Run the parser on the first element of a Haskell list of "WellFormedSExpr" values,
--   failing if the list is empty. This is useful in conjunction with the `asList`
--   function.
car :: (WellFormedSExpr t -> Either String t')
    -> [WellFormedSExpr t] -> Either String t'
car f (x:_) = f x
car _ []    = Left "car: Taking car of zero-element list"

-- | Run the parser on all but the first element of a Haskell list of "WellFormedSExpr" values,
--   failing if the list is empty. This is useful in conjunction with the `asList`
--   function.
cdr :: ([WellFormedSExpr t] -> Either String t')
    -> [WellFormedSExpr t] -> Either String t'
cdr f (_:xs) = f xs
cdr _ []     = Left "cdr: Taking cdr of zero-element list"
