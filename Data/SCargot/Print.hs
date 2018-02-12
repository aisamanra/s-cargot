{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SCargot.Print
         ( -- * Pretty-Printing
           encodeOne
         , encode
           -- * Pretty-Printing Control
         , SExprPrinter
         , Indent(..)
         , setFromCarrier
         , setMaxWidth
         , removeMaxWidth
         , setIndentAmount
         , setIndentStrategy
           -- * Default Printing Strategies
         , basicPrint
         , flatPrint
         ) where

import           Control.Applicative
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

import           Data.SCargot.Repr

-- | The 'Indent' type is used to determine how to indent subsequent
--   s-expressions in a list, after printing the head of the list.
data Indent
  = Swing -- ^ A 'Swing' indent will indent subsequent exprs some fixed
          --   amount more than the current line.
          --
          --   > (foo
          --   >   bar
          --   >   baz
          --   >   quux)
  | SwingAfter Int -- ^ A 'SwingAfter' @n@ indent will try to print the
                   --   first @n@ expressions after the head on the same
                   --   line as the head, and all after will be swung.
                   --   'SwingAfter' @0@ is equivalent to 'Swing'.
                   --
                   --   > (foo bar
                   --   >   baz
                   --   >   quux)
  | Align -- ^ An 'Align' indent will print the first expression after
          --   the head on the same line, and subsequent expressions will
          --   be aligned with that one.
          --
          --   > (foo bar
          --   >      baz
          --   >      quux)
    deriving (Eq, Show)

-- | A 'SExprPrinter' value describes how to print a given value as an
--   s-expression. The @carrier@ type parameter indicates the value
--   that will be printed, and the @atom@ parameter indicates the type
--   that will represent tokens in an s-expression structure.
data SExprPrinter atom carrier = SExprPrinter
  { atomPrinter  :: atom -> Text
      -- ^ How to serialize a given atom to 'Text'.
  , fromCarrier  :: carrier -> SExpr atom
      -- ^ How to turn a carrier type back into a 'Sexpr'.
  , swingIndent  :: SExpr atom -> Indent
      -- ^ How to indent subsequent expressions, as determined by
      --   the head of the list.
  , indentAmount :: Int
      -- ^ How much to indent after a swung indentation.
  , maxWidth     :: Maybe Int
      -- ^ The maximum width (if any) If this is 'None' then
      --   the resulting s-expression will always be printed
      --   on a single line.
  }

-- | A default 'LayoutOptions' struct that will always print a 'SExpr'
--   as a single line.
flatPrint :: (atom -> Text) -> SExprPrinter atom (SExpr atom)
flatPrint printer = SExprPrinter
  { atomPrinter  = printer
  , fromCarrier  = id
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Nothing
  }

-- | A default 'LayoutOptions' struct that will always swing subsequent
--   expressions onto later lines if they're too long, indenting them
--   by two spaces.
basicPrint :: (atom -> Text) -> SExprPrinter atom (SExpr atom)
basicPrint printer = SExprPrinter
  { atomPrinter  = printer
  , fromCarrier  = id
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Just 80
  }

-- | Modify the carrier type of a 'SExprPrinter' by describing how
--   to convert the new type back to the previous type. For example,
--   to pretty-print a well-formed s-expression, we can modify the
--   'SExprPrinter' value as follows:
--
-- >>> let printer = setFromCarrier fromWellFormed (basicPrint id)
-- >>> encodeOne printer (WFSList [WFSAtom "ele", WFSAtom "phant"])
-- "(ele phant)"
setFromCarrier :: (c -> b) -> SExprPrinter a b -> SExprPrinter a c
setFromCarrier fc pr = pr { fromCarrier = fromCarrier pr . fc }

-- | Dictate a maximum width for pretty-printed s-expressions.
--
-- >>> let printer = setMaxWidth 8 (basicPrint id)
-- >>> encodeOne printer (L [A "one", A "two", A "three"])
-- "(one \n  two\n  three)"
setMaxWidth :: Int -> SExprPrinter atom carrier -> SExprPrinter atom carrier
setMaxWidth n pr = pr { maxWidth = Just n }

-- | Allow the serialized s-expression to be arbitrarily wide. This
--   makes all pretty-printing happen on a single line.
--
-- >>> let printer = removeMaxWidth (basicPrint id)
-- >>> encodeOne printer (L [A "one", A "two", A "three"])
-- "(one two three)"
removeMaxWidth :: SExprPrinter atom carrier -> SExprPrinter atom carrier
removeMaxWidth pr = pr { maxWidth = Nothing }

-- | Set the number of spaces that a subsequent line will be indented
--   after a swing indentation.
--
-- >>> let printer = setMaxWidth 12 (basicPrint id)
-- >>> encodeOne printer (L [A "elephant", A "pachyderm"])
-- "(elephant \n  pachyderm)"
-- >>> encodeOne (setIndentAmount 4) (L [A "elephant", A "pachyderm"])
-- "(elephant \n    pachyderm)"
setIndentAmount :: Int -> SExprPrinter atom carrier -> SExprPrinter atom carrier
setIndentAmount n pr = pr { indentAmount = n }

-- | Dictate how to indent subsequent lines based on the leading
--   subexpression in an s-expression. For details on how this works,
--   consult the documentation of the 'Indent' type.
--
-- >>> let indent (A "def") = SwingAfter 1; indent _ = Swing
-- >>> let printer = setIndentStrategy indent (setMaxWidth 8 (basicPrint id))
-- >>> encodeOne printer (L [ A "def", L [ A "func", A "arg" ], A "body" ])
-- "(def (func arg)\n  body)"
-- >>> encodeOne printer (L [ A "elephant", A "among", A "pachyderms" ])
-- "(elephant \n  among\n  pachyderms)"
setIndentStrategy :: (SExpr atom -> Indent) -> SExprPrinter atom carrier -> SExprPrinter atom carrier
setIndentStrategy st pr = pr { swingIndent = st }

-- Sort of like 'unlines' but without the trailing newline
joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

-- Indents a line by n spaces
indent :: Int -> Text -> Text
indent n ts = T.replicate n " " <> ts

-- Indents every line n spaces, and adds a newline to the beginning
-- used in swung indents
indentAll :: Int -> [Text] -> Text
indentAll n = ("\n" <>) . joinLines . map (indent n)

-- Indents every line but the first by some amount
-- used in aligned indents
indentSubsequent :: Int -> [Text] -> Text
indentSubsequent _ [] = ""
indentSubsequent _ [t] = t
indentSubsequent n (t:ts) = joinLines (t : go ts)
  where go = map (indent n)

-- oh god this code is so disgusting
-- i'm sorry to everyone i let down by writing this
-- i swear i'll do better in the future i promise i have to
-- for my sake and for everyone's

-- | Pretty-print a 'SExpr' according to the options in a
--   'LayoutOptions' value.
prettyPrintSExpr :: SExprPrinter a (SExpr a) -> SExpr a -> Text
prettyPrintSExpr pr@SExprPrinter { .. } expr = case maxWidth of
  Nothing -> flatPrintSExpr (fmap atomPrinter (fromCarrier expr))
  Just w  -> indentPrintSExpr2 pr w expr

indentPrintSExpr :: SExprPrinter a (SExpr a) -> Int -> SExpr a -> Text
indentPrintSExpr SExprPrinter { .. } _ = pHead 0
  where
    pHead _   SNil         = "()"
    pHead _   (SAtom a)    = atomPrinter a
    pHead ind (SCons x xs) = gather ind x xs id
    gather ind h (SCons x xs) k = gather ind h xs (k . (x:))
    gather ind h end          k = "(" <> hd <> body <> tl <> ")"
      where tl   = case end of
                     SNil      -> ""
                     SAtom a   -> " . " <> atomPrinter a
                     SCons _ _ -> error "[unreachable]"
            hd   = indentSubsequent ind [pHead (ind+1) h]
            lst  = k []
            flat = T.unwords (map (pHead (ind+1)) lst)
            headWidth = T.length hd + 1
            indented =
              case swingIndent h of
                SwingAfter n ->
                  let (l, ls) = splitAt n lst
                      t  = T.unwords (map (pHead (ind+1)) l)
                      ts = indentAll (ind + indentAmount)
                           (map (pHead (ind + indentAmount)) ls)
                  in t <> ts
                Swing ->
                  indentAll (ind + indentAmount)
                  (map (pHead (ind + indentAmount)) lst)
                Align ->
                  indentSubsequent (ind + headWidth + 1)
                  (map (pHead (ind + headWidth + 1)) lst)
            body
              | length lst == 0              = ""
              | Just maxAmt <- maxWidth
              , T.length flat + ind > maxAmt = " " <> indented
              | otherwise = " " <> flat

-- | Pretty-printing for S-Expressions.  The general strategy is that
-- an SCons tail should either all fit on the current line, or else
-- each tail item should be placed on its own line with indenting.
-- Note that a line must print something, so while subsequent elements
-- will be placed on following lines, it is possible that the first
-- thing on a line (plus its indentation) may exceed the maxwidth.

data PPS = PPS { indentWc :: Int
               , remWidth :: Int
               , numClose :: Int
               }
         deriving Show

data SElem = SText Int T.Text
           | SPair Int SElem SElem
           | SDecl Int SElem [SElem]
           | SJoin Int [SElem]
             deriving (Show, Eq)

sElemSize :: SElem -> Int
sElemSize (SText n _) = n
sElemSize (SPair n _ _) = n
sElemSize (SDecl n _ _) = n
sElemSize (SJoin n _) = n

indentPrintSExpr2 :: SExprPrinter a (SExpr a) -> Int -> SExpr a -> Text
indentPrintSExpr2 SExprPrinter { .. } maxW sexpr =
    let atomTextTree = selems sexpr
        pretty = fmap addIndent $ fst $ pHead (PPS 0 maxW 0) atomTextTree
        -- prettyWithDebug = pretty <> ["", (T.pack $ show atomTextTree)]
    in T.unlines pretty
  where
    -- selems converts the (SExpr a) into an SElem, converting
    -- individual atoms to their text format but not concerned with
    -- other text formatting.  The resulting SElem tree will be
    -- iterated over to determine the wrapping strategy to apply.
    selems SNil = SText 2 "()"
    selems (SAtom a) = let p = atomPrinter a in SText (T.length p) p
    selems (SCons l r) =
        let l' = selems l
            lsz = sElemSize l'
            r' = selems r
            rsz = sElemSize r'
            bsz = lsz + rsz
        in case r of
             SNil -> SJoin lsz [l']
             SAtom _ -> SPair bsz l' r'
             _ -> case l of
                    SAtom _ -> case r' of
                                 SJoin _ rl' -> SDecl bsz l' rl'
                                 SDecl _ d dl -> SDecl bsz l' (d:dl)
                                 _ -> SDecl bsz l' [r']
                    _ -> SJoin bsz $ prefixOnto l' r'

    prefixOnto e (SJoin _ l) = e:l
    prefixOnto e (SDecl _ l r) = e:l:r
    prefixOnto e r = [e,r]

    addIndent (Nothing, t) = t
    addIndent (Just n, t) = indent n t

    pHead :: PPS -> SElem -> ( [(Maybe Int, Text)], PPS )
    pHead pps (SText _ t) = ( [(Nothing, t)]
                            , pps { remWidth = remWidth pps - T.length t})
    pHead pps (SPair _ e1 e2) =
        let (t1,pps1) = pHead pps e1
            (t2,pps2) = pTail ppsNextLine e2
            (t3,pps3) = pTail ppsSameLine e2  -- same line
            ppsNextLine = pps { remWidth = remWidth pps - T.length sep }
            ppsSameLine = pps1 { remWidth = remWidth pps1 - T.length sep }
            sep = " . "
            t1h = head t1
            wrapJoin i l rs = wrapT i (snd l <> sep) rs
            sameLine l r p = (wrapJoin (indentWc pps) l r, p)
            separateLines l r p = (wrapTWith False "(" "" (indentWc pps) "" l ++
                                   wrapTWith True sep ")" (indentWc pps) "" r, p)
        in if length t1 > 1 || remWidth pps3 < numClose pps + 5
           then separateLines t1 t2 pps2
           else sameLine t1h t3 pps3
    -- An SJoin is a sequence of elements at the same rank.  They are
    -- either all placed on a single line, or one on each line.
    pHead pps (SJoin _ []) = ( [], pps )
    pHead pps (SJoin els others) =
        let (t1,_) = pHead pps $ head others
            (t3,pps3) = foldl pTail' ([], pps) others
            pTail' :: ([(Maybe Int, Text)], PPS) -> SElem -> ([(Maybe Int, Text)], PPS)
            pTail' (rl,pp) ne = let (rt,pr) = pTail pp ne
                                    hrl = head rl
                                    hrt = head rt
                                in if length rt == 1
                                   then case length rl of
                                          0 -> (rt, pr)
                                          1 -> ((fst hrl, snd hrl <> " " <> snd hrt):[], pr)
                                          _ -> (rl <> rt, pr)
                                   else (rl <> rt, pr)
            sameLine parts pEnd = (wrapT (indentWc pps) "" parts, pEnd)
            separateLines elems pEnd =
                let lr = concatMap (fst . pTail pEnd) elems
                in (wrapTWith False "(" ")" (indentWc pps) "" lr, pEnd)
        in if els > remWidth pps3 || length t1 > 1 || remWidth pps3 < numClose pps + 5
           then separateLines others pps
           else sameLine t3 pps3
    --  For an SDecl, always put the first element on the line.  If
    --  *all* other elements fit on the same line, do that, otherwise
    --  all other elements should appear on subsequent lines with
    --  indentation.  This will produce left-biased wrapping: wrapping
    --  will occur near the root of the SExp tree more than at the
    --  leaves.
    pHead pps (SDecl els first others) =
        let (t1,pps1) = pHead pp2 first
            (to1,_) = pTail pps1 (head others)
            firstPlusFits = sElemSize first + sElemSize (head others) < (remWidth pps - 4)
            allFits = els < (remWidth pps - length others - 3)
            pfxLen = indentAmount
            tryFirstArgSameLine = case swingIndent (SCons SNil (SCons SNil SNil)) of
                                    Align -> True
                                    _ -> False
            pp2 = pps { indentWc = indentWc pps + pfxLen
                      , remWidth = remWidth pps - 1 - pfxLen
                      , numClose = numClose pps + 1
                      }
            t1h = head t1
            pps1' = pps1 { indentWc = indentWc pps1 + T.length (snd t1h) + 1
                         , remWidth = remWidth pps1 - T.length (snd t1h) - 1
                         }
            tothers = concatMap (fst . pTail pp2) others -- multiline
            tothers' = concatMap (fst . pTail pps1') $ tail others -- multiline from 2nd
            (others', ppone) = foldl foldPTail ([],pps1) others -- oneline
            (others'', ppone') = foldl foldPTail ([],pps1') $ tail others -- multiline from 2nd
            foldPTail (tf,ppf) o = let (ot,opp) = pTail ppf o
                                       tf1 = head tf
                                       tr = if length ot == 1
                                            then case length tf of
                                                   0 -> ot
                                                   1 -> [(fst tf1, snd tf1 <> " " <> snd (head ot))]
                                                   _ -> tf ++ ot
                                            else tf ++ ot
                                   in (tr, opp)
            separateLines l r p =
                let wr = if null r then []
                         else wrapTWith True "" ")" (indentWc p) "" r
                    cl = if null r then ")" else ""
                in (wrapTWith False "(" cl (indentWc pps) "" l <> wr, pps)
            maybeSameLine l (r1,p1) (rn,p2) =
                if length r1 <= 1 && remWidth p1 > numClose p1
                then (wrapT (indentWc pps) (snd l <> " ") r1, p1)
                else separateLines [l] rn p2
        in if allFits && length t1 < 2
           then maybeSameLine t1h (others',ppone) (tothers,pp2)
           else if (tryFirstArgSameLine && firstPlusFits &&
                    length t1 < 2 &&
                    length to1 < 2 &&
                    not (null to1) && not (null others))
                then maybeSameLine (fst t1h,
                                    snd t1h <> " " <> snd (head to1)) (others'',ppone') (tothers',pps1')
                else separateLines t1 tothers pp2


    pTail = pHead


wrapTWith :: Bool -> T.Text -> T.Text -> Int
          -> T.Text
          -> [(Maybe Int, T.Text)]
          -> [(Maybe Int, T.Text)]
wrapTWith isContinued st en ind hstart ts =
    let th = head ts
        tt = last ts
        tb = init $ tail ts
        tp l = (fst l <|> Just ind, snd l)
        fi = if isContinued then Just ind else Nothing
    in if length ts > 1
       then (((fi, st <> hstart <> snd th) : map tp tb) ++
             [ tp $ (fst tt, snd tt <> en) ])
       else [(fi, st <> hstart <> snd th <> en)]

wrapT :: Int -> T.Text -> [(Maybe Int, T.Text)] -> [(Maybe Int, T.Text)]
wrapT = wrapTWith False "(" ")"


-- if we don't indent anything, then we can ignore a bunch of the
-- details above
flatPrintSExpr :: SExpr Text -> Text
flatPrintSExpr = TL.toStrict . B.toLazyText . pHead
  where
    pHead (SCons x xs) =
      B.fromString "(" <> pHead x <> pTail xs
    pHead (SAtom t)    =
      B.fromText t
    pHead SNil         =
      B.fromString "()"

    pTail (SCons x xs) =
      B.fromString " " <> pHead x <> pTail xs
    pTail (SAtom t) =
      B.fromString " . " <> B.fromText t <> B.fromString ")"
    pTail SNil =
      B.fromString ")"

-- | Turn a single s-expression into a string according to a given
--   'SExprPrinter'.
encodeOne :: SExprPrinter atom carrier -> carrier -> Text
encodeOne s@(SExprPrinter { .. }) =
  prettyPrintSExpr (s { fromCarrier = id }) . fromCarrier

-- | Turn a list of s-expressions into a single string according to
--   a given 'SExprPrinter'.
encode :: SExprPrinter atom carrier -> [carrier] -> Text
encode spec = T.intercalate "\n\n" . map (encodeOne spec)
