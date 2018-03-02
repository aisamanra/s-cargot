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
  Just _  -> indentPrintSExpr pr expr

indentPrintSExpr :: SExprPrinter a (SExpr a) -> SExpr a -> Text
indentPrintSExpr SExprPrinter { .. } = pHead 0
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

  -- where
  --   -- this is the base-case that knows how to print empty lists and
  --   -- atoms
  --   pHead _   SNil         = B.fromString "()"
  --   pHead _   (SAtom a)    = B.fromText a
  --   pHead ind (SCons x xs) = gather ind x xs id 0

  --   -- otherwise, we trawl through the list grabbing every element...
  --   gather ind h (SCons x xs) k r = gather ind h xs (k . (x:)) (r + T.length x)
  --   gather ind h end          k r = B.fromString "(" <> hd <> body <> tl <> B.fromString ")"
  --     where
  --       tl   = case end of
  --                SNil      -> mempty
  --                SAtom a   -> B.fromString " . " <> B.fromText a
  --                SCons _ _ -> error "[unreachable]"
  --       hd   = indentSubsequent ind [pHead (ind+1) h]
  --       lst  = k []
  --       flat = T.unwords (map (pHead (ind+1)) lst)
  --       headWidth = T.length hd + 1
  --       indented =
  --         case swingIndent h of
  --           SwingAfter n ->
  --             let (l, ls) = splitAt n lst
  --                 t  = T.unwords (map (pHead (ind+1)) l)
  --                 ts = indentAll (ind + indentAmount)
  --                                (map (pHead (ind + indentAmount)) ls)
  --             in t <> ts
  --           Swing ->
  --             indentAll (ind + indentAmount)
  --               (map (pHead (ind + indentAmount)) lst)
  --           Align ->
  --             indentSubsequent (ind + headWidth + 1)
  --               (map (pHead (ind + headWidth + 1)) lst)
  --       body
  --         | length lst == 0              = B.fromString ""
  --         | Just maxAmt <- maxWidth
  --         , T.length flat + ind > maxAmt = B.fromString " " <> indented
  --         | otherwise                    = B.fromString " " <> flat

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
