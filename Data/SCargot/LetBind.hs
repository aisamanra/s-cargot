{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SCargot.LetBind
    ( -- $intro
      -- * Automatically finding let bindings
      discoverLetBindings
    , DiscoveryGuide(..)
    , nativeGuide
      -- * Expanding
    -- (insert let-bound variables and remove variable section)
    , letExpand
    )
    where

import           Control.Applicative
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.SCargot.Repr
import           Data.String
import qualified Data.Traversable as T


data DiscoveryGuide a str = Guide
    { maxLetBinds :: Int -> Int
      -- ^ Maximum number of let bindings to generate.  Given the
      -- total number discovered as input to allow the maximum number
      -- to be intelligently determined.

    , minExprSize :: Int
      -- ^ Minimum sexpr size to be considered for a let variable
      -- binding.  Expressions shorter than this will not be
      -- let-bound.

    , allowRecursion :: Bool
      -- ^ Allow rec bindings, or just direct let bindings?

    , weighting :: SExpr a -> Int -> Int
      -- ^ Given an SExpr sub-expression and the count of occurrences
      -- of that sub-expression, return a weighting value that is used
      -- for sorting the discovered let bindings to choose the most
      -- weighty 'maxLetBinds' for substitution.

    , letMaker :: (IsString str) => str -> a
      -- ^ Called to generate the "let" statement token itself

    , labelMaker :: (IsString str, Monoid str) => str -> a
      -- ^ Called to generate the binding variable name token given the
      -- name.
    }


nativeGuide :: (str -> a) -> (str -> a) -> DiscoveryGuide a str
nativeGuide letMk labelMk = Guide { maxLetBinds = const 10
                                  , minExprSize = 6
                                  , allowRecursion = False
                                  , weighting = \s cnt -> let h = F.length s
                                                          in (h + (cnt * 4))
                                  , letMaker = letMk
                                  , labelMaker = labelMk
                                  }

discoverLetBindings :: (Monoid str, IsString str, Eq a) =>
                        DiscoveryGuide a str -> SExpr a -> SExpr a
discoverLetBindings guide inp =
    let (inpMap,annotInp) = explore guide startingLoc inp
                    -- KWQ: locs rec v.s. let selections?
        locs = bestBindings guide annotInp $ points inpMap
        lbn = assignLBNames guide inp locs
        letPart = SAtom $ letMaker guide "let"
        (lbvdefs, subsInp) = substLBRefs guide lbn annotInp
    in SCons letPart $ SCons lbvdefs (SCons subsInp SNil)

{- $ intro

This module allows let bindings to be introduced into the S-Expression
syntax.  The typical use is to add let bindings before serializing to
disk, and then expand the bindings after reading from the disk but
before passing to other processing; this process allows the
application using the S-Expressions to be unaware of the let-binding
compression, although it does not obtain corresponding advantages of
the re-use of let-bound variables.

The 'discoverLetBindings' function can be called to automatically
assign let bindings based on a weighting algorithm of discovered
S-expression phrases.  The discovery is guided by parameters provided
by the caller in the 'DiscoveryGuide'; this guide also provides the
functions used to create the variables and the top-level let statement
in the language of the current S-expression.

The 'weighting' function of the 'DiscoveryGuide' can be used to assign
weights to various S-expression phrases: the S-expressions with the
highest weights will be let-bound to variables (up to the
'maxLetBinds' limit).

-}

bestBindings :: DiscoveryGuide a str -> ExprInfo a -> [Location a] -> [Location a]
bestBindings guide exprs locs = getMaxBest
    where getMaxBest = last $
                       sortBy (compare `on` length) $
                       fmap getBestSkipping [0..maxbinds*2]
          getBestSkipping n = snd $ snd $
                              foldl bestB ((n, 0), (maxbinds, [])) $
                              reverse $
                              sortBy (compare `on` (uncurry (weighting guide) . lwi)) $
                              filter ((/=) 0 . locCount) locs
          bestB (_, (0, b)) _ = ((0,0), (0, b))
          bestB ((s,h), (n, b)) e = if not (allowRecursion guide) && isSubBinding e b
                                    then ((s, h), (n, b))
                                    else if s > 0
                                         then ((s-1, h), (n, b))
                                         else ((s, h), (n-1, e:b))
          isSubBinding :: Location a -> [Location a] -> Bool
          isSubBinding x = or . fmap (isSub x)
          isSub :: Location a -> Location a -> Bool
          isSub x startingFrom = isJust (findLocation (locId startingFrom) exprs >>=
                                         findLocation (locId x))
          lwi l = (locExpr l, locCount l)
          maxbinds = maxLetBinds guide (length locs)

type LocationId = Int
data Location a = Location { locExpr :: SExpr a
                           , locCount :: Int
                           , locId :: LocationId
                           }
                deriving Show
data NamedLoc a = NamedLoc { nlocId :: LocationId
                           , nlocVar :: SExpr a
                           }

data MyMap a = MyMap { points :: [Location a]
                     }
startingLoc = MyMap []

data ExprInfo a = EINil | EIAtom a | EICons LocationId (ExprInfo a) (ExprInfo a)

explore :: Eq a => DiscoveryGuide a str -> MyMap a -> SExpr a -> (MyMap a, ExprInfo a)
explore guide mymap SNil = (mymap, EINil)
explore guide mymap (SAtom a) = (mymap, EIAtom a)
explore guide mymap h@(SCons l r) =
    let (lc,le) = explore guide mymap l
        (rc,re) = explore guide lc r
        (hm,hi) = updateMap guide h rc
    in (hm, EICons hi le re)


updateMap :: Eq a => DiscoveryGuide a str -> SExpr a -> MyMap a -> (MyMap a, LocationId)
updateMap guide point mymap =
    let (p, i) = addOrUpdate (points mymap)
    in (mymap { points = p }, i)
    where addOrUpdate [] = ([ Location { locExpr=point, locCount=succCnt(0), locId=lId} ], lId)
          addOrUpdate (p:ps) = let (sm,si) = addOrUpdate ps
                               in if locExpr p /= point
                                  then (p : sm, si)
                                  else (p { locCount = succCnt(locCount p) } : ps, locId p)
          lId = length (points mymap)
          succCnt n = if F.length point > (minExprSize guide) then n + 1 else n  -- ignore short SExprs


findLocation :: LocationId -> ExprInfo a -> Maybe (ExprInfo a)
findLocation loc = fndLoc
    where fndLoc EINil = Nothing
          fndLoc (EIAtom _) = Nothing
          fndLoc e@(EICons el l r) = if el == loc then Just e else fndLoc l <|> fndLoc r

assignLBNames :: (Eq a, IsString str, Monoid str) =>
                 DiscoveryGuide a str -> SExpr a -> [Location a] -> [NamedLoc a]
assignLBNames guide inp = snd . T.mapAccumL mkNamedLoc 0
    where mkNamedLoc i l = let nm = labelMaker guide $ "var" <> fromString (show i)
                           in case F.find ((==) nm) inp of
                                Nothing -> (i+1, NamedLoc { nlocId = locId l
                                                          , nlocVar = SAtom nm
                                                          })
                                Just _ -> mkNamedLoc (i+1) l  -- collision, try another varname

substLBRefs :: DiscoveryGuide a str -> [NamedLoc a] -> ExprInfo a -> (SExpr a, SExpr a)
substLBRefs guide nlocs = subsRefs SNil
    where subsRefs b EINil = (b, SNil)
          subsRefs b (EIAtom a) = (b, SAtom a)
          subsRefs b (EICons i l r) = let (b',l') = subsRefs b l
                                          (c',r') = subsRefs b' r
                                          here = SCons l' r'
                                      in case hasBinding i of
                                           Nothing -> (c', here)
                                           Just loc -> (addVar c' (nlocVar loc) here, (SCons (nlocVar loc) SNil))
          hasBinding i = F.find ((==) i . nlocId) nlocs
          addVar vl vn vv = SCons (SCons vn (SCons vv SNil)) vl


-- ----------------------------------------------------------------------

letExpand :: (Eq a, Show a, Eq str, IsString str) => (a -> Maybe str) -> SExpr a -> SExpr a
letExpand atomToText = findExpLet
    where findExpLet e@(SCons (SAtom a) (SCons lbvdefs (SCons subsInp SNil))) =
              if atomToText a == Just "let"
              then expLet lbvdefs subsInp
              else SCons (SAtom a) (SCons (findExpLet lbvdefs) (SCons (findExpLet subsInp) SNil))
          findExpLet e = e
          expLet lb = expandWith (bindings lb)
          bindings = parseVar []
          parseVar vdefs (SCons (SCons vn (SCons vv SNil)) r) = (vn, vv) : parseVar vdefs r
          parseVar vdefs SNil = vdefs
          parseVar vdefs e = error $ "Expected a var, got: " <> show e
          expandWith _ SNil = SNil
          expandWith vdefs e@(SCons v@(SAtom _) SNil) =
              case lookup v vdefs of
                Nothing -> e
                Just vv -> expandWith vdefs vv
          expandWith vdefs e@(SCons l r) =
              case lookup e vdefs of
                Nothing -> SCons (expandWith vdefs l) (expandWith vdefs r)
                Just vv -> expandWith vdefs vv
          expandWith vdefs e@(SAtom _) = e
