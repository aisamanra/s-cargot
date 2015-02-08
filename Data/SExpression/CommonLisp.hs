-- | Contains the type of atoms that Common Lisp understands, as
--   well as the built-in reader macros that Common Lisp provides.
--   Given a Common Lisp source file that contains no extra reader
--   macro definitions, this module should successfully parse and
--   desugar even quoted lists and vector literals.

module Data.SExpression.CommonLisp where

data Atom
  = Symbol Text
  | String Text
  | Integer Int
  | True
    deriving (Eq, Show, Read)

parseSexpr :: Text -> Either SExprError 
