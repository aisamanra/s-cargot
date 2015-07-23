module Data.SCargot.Pretty where


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
  { atomPrinter  :: a -> Text -- ^ How to serialize a given atom to 'Text'.
  , swingIndent  :: a -> Bool -- ^ Whether or not to swing
  , indentAmount :: Int       -- ^ How much to indent after a swing
  , maxWidth     :: Maybe Int -- ^ The maximum width (if any)
  }

basicPrint :: (a -> Text) -> LayoutOptions a
basicPrint f = LayoutOptions
  { atomPrinter = f
  , swingIndent = const False
  , maxWidth    = Nothing
  }

prettyPrintSExpr :: LayoutOptions a -> SExpr a -> Text
