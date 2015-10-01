{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SCargot.Pretty
         ( -- * Pretty-Printing
           prettyPrintSExpr
           -- * Pretty-Printing Control
         , LayoutOptions(..)
         , Indent(..)
           -- * Default Printing Strategies
         , basicPrint
         , flatPrint
         ) where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

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

-- | A 'LayoutOptions' value describes the strategy taken in
--   pretty-printing a 'SExpr'.
data LayoutOptions a = LayoutOptions
  { atomPrinter  :: a -> Text
      -- ^ How to serialize a given atom to 'Text'.
  , swingIndent  :: SExpr a -> Indent
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
flatPrint :: (a -> Text) -> LayoutOptions a
flatPrint printer = LayoutOptions
  { atomPrinter  = printer
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Nothing
  }

-- | A default 'LayoutOptions' struct that will always swing subsequent
--   expressions onto later lines if they're too long, indenting them
--   by two spaces.
basicPrint :: (a -> Text) -> LayoutOptions a
basicPrint printer = LayoutOptions
  { atomPrinter  = printer
  , swingIndent  = const Swing
  , indentAmount = 2
  , maxWidth     = Just 80
  }

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
prettyPrintSExpr :: LayoutOptions a -> SExpr a -> Text
prettyPrintSExpr LayoutOptions { .. } = pHead 0
  where pHead _   SNil         = "()"
        pHead _   (SAtom a)    = atomPrinter a
        pHead ind (SCons x xs) = gather ind x xs id
        gather _   _ (SAtom _)    _ = error "no dotted pretty printing yet!"
        gather ind h (SCons x xs) k = gather ind h xs (k . (x:))
        gather ind h SNil         k = "(" <> hd <> body <> ")"
          where hd   = indentSubsequent ind [pHead (ind+1) h]
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
                  | otherwise                    = " " <> flat
