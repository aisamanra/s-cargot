-- | Contains the type of atoms that Common Lisp understands, as
--   well as the built-in reader macros that Common Lisp provides.
--   Given a Common Lisp source file that contains no extra reader
--   macro definitions, this module should successfully parse and
--   desugar even quoted lists and vector literals.

module Data.SCargot.CommonLisp where

data CLAtom
  = CLSymbol Text
  | CLString Text
  | CLInteger Integer
  | CLRatio Integer Integer
  | CLFloat Double
    deriving (Eq, Show, Read)

type CommonLispSpec carrier = SExprSpec CLAtom carrier

withComments :: CommonLispSpec c -> CommonLispSpec c
withComments = addCommentType (const () <$> (char ';' *> restOfLine))

withQuote :: CommonLispSpec (SCons CLAtom) -> CommonLispSpec (SCons CLAtom)
withQuote = addReader '\'' (go <$> parse)
  where go v = SCons q (SCons v SNil)

-- | Adds support for the '#(...)' sugar for vectors. (This will be
--   parsed as '(vector ...)', and
withVectors :: CommonLispSpec c -> CommonLispSpec c
withVectors = addReader '#' (go <$> parse)

parse :: CommonLispSpec c -> Text -> Either String c
serialize :: CommonLispSpec c -> c -> Text
