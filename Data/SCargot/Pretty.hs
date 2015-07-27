{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SCargot.Pretty
         ( LayoutOptions(..)
         , basicPrint
         , flatPrint
         , prettyPrintSExpr
         ) where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.SCargot.Repr

-- | A 'LayoutOptions' value describes how to pretty-print a 'SExpr'.
--   It describes how to print atoms, what horizontal space to fit
--   it into, and other related options.
--
--   The 'swingIndent' value might require a big of explanation: in
--   pretty-printing s-expressions, you have the option of whether
--   to 'swing' expressions which get pushed to subsequent lines
--   to the left, or to align them along the right. e.g. the
--   s-expression @(foo a b)@ could use a non-swing indent as
--
--   > (foo arg-one
--   >      arg-two)
--
--   or a swing indent as
--
--   > (foo arg-one
--   >   arg-two)
--
--   often, in formatting Lisp code, control structures will
--   swing subsequent expressions, as in
--
--   > (define (factorial n)
--   >   (if (= n 0)
--   >     1
--   >     (* n (fact (- n 1)))))
--
--   but most functions will _not_ swing:
--
--   > (call-my-func arg-number-one
--   >               arg-number-two
--   >               arg-number-three)
--
--   The 'swingIndent' field lets you choose whether or not to
--   swing subsequent s-expressions based on the atom in the car
--   position of a list. You can default to always swinging subsequent
--   expressions with @const True@ and never with @const False@, or
--   choose based on some more advanced criteria. _If_ a swing happens,
--   subsequent lines are indented based on the 'indentAmount' variable;
--   otherwise, subsequent lines are indented based on the size of the
--   @car@ of the list.
data LayoutOptions a = LayoutOptions
  { atomPrinter  :: a -> Text       -- ^ How to serialize a given atom to 'Text'.
  , swingIndent  :: SExpr a -> Bool -- ^ Whether or not to swing
  , indentAmount :: Int             -- ^ How much to indent after a swing
  , maxWidth     :: Maybe Int       -- ^ The maximum width (if any)
  }

flatPrint :: (a -> Text) -> LayoutOptions a
flatPrint printer = LayoutOptions
  { atomPrinter  = printer
  , swingIndent  = const True
  , indentAmount = 2
  , maxWidth     = Nothing
  }

basicPrint :: (a -> Text) -> LayoutOptions a
basicPrint printer = LayoutOptions
  { atomPrinter  = printer
  , swingIndent  = const True
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
indentAll :: Int -> [Text] -> Text
indentAll n = ("\n" <>) . joinLines . map (indent n)

-- Indents every line but the first by some amount
indentSubsequent :: Int -> [Text] -> Text
indentSubsequent _ [] = ""
indentSubsequent _ [t] = t
indentSubsequent n (t:ts) = joinLines (t : go ts)
  where go = map (indent n)

-- oh god this code is so disgusting
-- i'm sorry to everyone i let down by writing this
-- i swear i'll do better in the future i promise i have to
-- for my sake and for everyone's
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
                indented
                  | swingIndent h =
                      indentAll (ind + indentAmount)
                        (map (pHead (ind + indentAmount)) lst)
                  | otherwise =
                      indentSubsequent (ind + headWidth + 1)
                        (map (pHead (ind + headWidth + 1)) lst)
                body | length lst == 0                = ""
                     | Just maxAmt <- maxWidth
                     , (T.length flat + ind) > maxAmt = " " <> indented
                     | otherwise                      = " " <> flat
