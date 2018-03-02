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
         , unconstrainedPrint
         ) where

import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Traversable as T

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
      -- ^ The maximum width (if any) If this is 'None' then the
      --   resulting s-expression might be printed on one line (if
      --   'indentPrint' is 'False') and might be pretty-printed in
      --   the most naive way possible (if 'indentPrint' is 'True').
  , indentPrint :: Bool
      -- ^ Whether to indent or not. This has been retrofitted onto
  }


-- | A default 'SExprPrinter' struct that will always print a 'SExpr'
--   as a single line.
flatPrint :: (atom -> Text) -> SExprPrinter atom (SExpr atom)
flatPrint printer = SExprPrinter
  { atomPrinter  = printer
  , fromCarrier  = id
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Nothing
  , indentPrint  = False
  }

-- | A default 'SExprPrinter' struct that will always swing subsequent
--   expressions onto later lines if they're too long, indenting them
--   by two spaces, and uses a soft maximum width of 80 characters
basicPrint :: (atom -> Text) -> SExprPrinter atom (SExpr atom)
basicPrint printer = SExprPrinter
  { atomPrinter  = printer
  , fromCarrier  = id
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Just 80
  , indentPrint  = True
  }

-- | A default 'SExprPrinter' struct that will always swing subsequent
-- expressions onto later lines if they're too long, indenting them by
-- two spaces, but makes no effort to keep the pretty-printed sources
-- inside a maximum width. In the case that we want indented printing
-- but don't care about a "maximum" width, we can print more
-- efficiently than in other situations.
unconstrainedPrint :: (atom -> Text) -> SExprPrinter atom (SExpr atom)
unconstrainedPrint printer = SExprPrinter
  { atomPrinter  = printer
  , fromCarrier  = id
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Nothing
  , indentPrint  = True
  }

-- | This is an intermediate representation which is like (but not
-- identical to) a RichSExpr representation. In particular, it has a
-- special case for empty lists, and it also keeps a single piece of
-- indent information around for each list
data Intermediate
  = IAtom Text
  | IList Indent Intermediate (Seq.Seq Intermediate) (Maybe Text)
  | IEmpty


toIntermediate :: SExprPrinter a (SExpr a) -> SExpr a -> Intermediate
toIntermediate
  SExprPrinter { atomPrinter = printAtom
               , swingIndent = swing
               } = headOf
  where
    headOf (SAtom a)    = IAtom (printAtom a)
    headOf SNil         = IEmpty
    headOf (SCons x xs) =
      gather (swing x) (headOf x) (Seq.empty) xs
    gather sw hd rs SNil =
      IList sw hd rs Nothing
    gather sw hd rs (SAtom a) =
      IList sw hd rs (Just (printAtom a))
    gather sw hd rs (SCons x xs) =
      gather sw hd (rs Seq.|> headOf x) xs


unboundIndentPrintSExpr :: SExprPrinter a (SExpr a) -> SExpr a -> TL.Text
unboundIndentPrintSExpr spec = finalize . go . toIntermediate spec
  where
    finalize = B.toLazyText . F.foldMap (<> B.fromString "\n")

    go :: Intermediate -> Seq.Seq B.Builder
    go (IAtom t) = Seq.singleton (B.fromText t)
    go IEmpty    = Seq.singleton (B.fromString "()")
    -- this case should never be called with an empty argument to
    -- @values@, as that should have been translated to @IEmpty@
    -- instead.
    go (IList iv initial values rest)
      -- if we're looking at an s-expression that has no nested
      -- s-expressions, then we might as well consider it flat and let
      -- it take the whole line
      | Just strings <- T.traverse ppBasic (initial Seq.<| values) =
        Seq.singleton (B.fromString "(" <> buildUnwords strings <> pTail rest)

      -- it's not "flat", so we might want to swing after the first thing
      | Swing <- iv =
          -- if this match fails, then it means we've failed to
          -- convert to an Intermediate correctly!
          let butLast = insertParen (go initial) <> fmap doIndent (F.foldMap go values)
          in handleTail rest butLast

      -- ...or after several things
      | SwingAfter n <- iv =
          let (hs, xs) = Seq.splitAt n (initial Seq.<| values)
              hd = B.fromString "(" <> buildUnwords (F.foldMap go hs)
              butLast = hd Seq.<| fmap doIndent (F.foldMap go xs)
          in handleTail rest butLast

      -- the 'align' choice is clunkier because we need to know how
      -- deep to indent, so we have to force the first builder to grab its size
      | otherwise =
        let -- so we grab that and figure out its length plus two (for
            -- the leading paren and the following space). This uses a
            -- max because it's possible the first thing is itself a
            -- multi-line s-expression (in which case it seems like
            -- using the Align strategy is a terrible idea, but who am
            -- I to quarrel with the wild fruits upon the Tree of
            -- Life?)
            len = 2 + F.maximum (fmap (TL.length . B.toLazyText) (go initial))
        in case Seq.viewl values of
          -- if there's nothing after the head of the expression, then
          -- we simply close it
          Seq.EmptyL -> insertParen (insertCloseParen (go initial))
          -- otherwise, we put the first two things on the same line
          -- with spaces and everything else gets indended the
          -- forementioned length
          y Seq.:< ys ->
            let hd = B.fromString "(" <> buildUnwords (F.foldMap go (Seq.fromList [initial, y]))
                butLast = hd Seq.<| fmap (doIndentOf (fromIntegral len)) (F.foldMap go ys)
            in handleTail rest butLast
            -- B.fromString "(" <> buildUnwords (F.foldMap go (Seq.fromList [x, y]))
            -- Seq.<| fmap (doIndentOf (fromIntegral len)) (handleTail rest (F.foldMap go ys))

    doIndent :: B.Builder -> B.Builder
    doIndent = doIndentOf (indentAmount spec)

    doIndentOf :: Int -> B.Builder -> B.Builder
    doIndentOf n b = B.fromText (T.replicate n " ") <> b

    insertParen :: Seq.Seq B.Builder -> Seq.Seq B.Builder
    insertParen s = case Seq.viewl s of
      Seq.EmptyL -> s
      x Seq.:< xs -> (B.fromString "(" <> x) Seq.<| xs

    handleTail :: Maybe Text -> Seq.Seq B.Builder -> Seq.Seq B.Builder
    handleTail Nothing = insertCloseParen
    handleTail (Just t) =
      (Seq.|> (B.fromString "." <> B.fromText t <> B.fromString ")"))

    insertCloseParen :: Seq.Seq B.Builder -> Seq.Seq B.Builder
    insertCloseParen s = case Seq.viewr s of
      Seq.EmptyR -> Seq.singleton (B.fromString ")")
      xs Seq.:> x -> xs Seq.|> (x <> B.fromString ")")

    buildUnwords sq =
      case Seq.viewl sq of
      Seq.EmptyL -> mempty
      t Seq.:< ts -> t <> F.foldMap (\ x -> B.fromString " " <> x) ts

    pTail Nothing = B.fromString ")"
    pTail (Just t) = B.fromString ". " <> B.fromText t <> B.fromString ")"

    ppBasic (IAtom t) = Just (B.fromText t)
    ppBasic (IEmpty) = Just (B.fromString "()")
    ppBasic _ = Nothing


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


-- Indents a line by n spaces
indent :: Int -> Text -> Text
indent n ts = T.replicate n " " <> ts


-- Sort of like 'unlines' but without the trailing newline
joinLinesS :: Seq.Seq Text -> Text
joinLinesS s = case Seq.viewl s of
  Seq.EmptyL -> ""
  t Seq.:< ts
    | F.null ts -> t
    | otherwise -> t <> "\n" <> joinLinesS ts


-- Sort of like 'unlines' but without the trailing newline
unwordsS :: Seq.Seq Text -> Text
unwordsS s = case Seq.viewl s of
  Seq.EmptyL -> ""
  t Seq.:< ts
    | F.null ts -> t
    | otherwise -> t <> " " <> joinLinesS ts


-- Indents every line n spaces, and adds a newline to the beginning
-- used in swung indents
indentAllS :: Int -> Seq.Seq Text -> Text
indentAllS n = ("\n" <>) . joinLinesS . fmap (indent n)


-- Indents every line but the first by some amount
-- used in aligned indents
indentSubsequentS :: Int -> Seq.Seq Text -> Text
indentSubsequentS n s = case Seq.viewl s of
  Seq.EmptyL -> ""
  t Seq.:< ts
    | F.null ts -> t
    | otherwise -> joinLinesS (t Seq.<| fmap (indent n) ts)
--      where go = fmap (indent n)


-- oh god this code is so disgusting
-- i'm sorry to everyone i let down by writing this
-- i swear i'll do better in the future i promise i have to
-- for my sake and for everyone's

-- | Pretty-print a 'SExpr' according to the options in a
--   'LayoutOptions' value.
prettyPrintSExpr :: SExprPrinter a (SExpr a) -> SExpr a -> Text
prettyPrintSExpr pr@SExprPrinter { .. } expr = case maxWidth of
  Nothing
    | indentPrint -> TL.toStrict (unboundIndentPrintSExpr pr (fromCarrier expr))
    | otherwise   -> flatPrintSExpr (fmap atomPrinter (fromCarrier expr))
  Just _  -> indentPrintSExpr' pr expr


indentPrintSExpr' :: SExprPrinter a (SExpr a) -> SExpr a -> Text
indentPrintSExpr' pr@SExprPrinter { .. } = pp 0 . toIntermediate pr
  where
    pp _   IEmpty         = "()"
    pp _   (IAtom t)    = t
    pp ind (IList i h values end) = "(" <> hd <> body <> tl <> ")"
      where
        tl = case end of
               Nothing -> ""
               Just x  -> " . " <> x
        hd = pp (ind+1) h
        flat = unwordsS (fmap (pp (ind + 1)) values)
        headWidth = T.length hd + 1
        indented =
          case i of
            SwingAfter n ->
              let (l, ls) = Seq.splitAt n values
                  t  = unwordsS (fmap (pp (ind+1)) l)
                  ts = indentAllS (ind + indentAmount)
                       (fmap (pp (ind + indentAmount)) ls)
              in t <> ts
            Swing ->
              indentAllS (ind + indentAmount)
                (fmap (pp (ind + indentAmount)) values)
            Align ->
              indentSubsequentS (ind + headWidth + 1)
                (fmap (pp (ind + headWidth + 1)) values)
        body
          | length values == 0 = ""
          | Just maxAmt <- maxWidth
          , T.length flat + ind > maxAmt = " " <> indented
          | otherwise = " " <> flat


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
