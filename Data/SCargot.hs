module Data.SCargot
  ( -- * SCargot Basics

    -- $intro

    -- * Parsing and Printing
    decode
  , decodeOne
  , encode
  , encodeOne
    -- * Parser Construction
    -- ** Specifying a Parser
  , SExprParser
  , Reader
  , Comment
  , mkParser
  , setCarrier
  , addReader
  , setComment
  , asRich
  , asWellFormed
  , withQuote
    -- * Printer Construction
    -- * Specifying a Pretty-Printer
  , SExprPrinter
  , Indent(..)
  , basicPrint
  , flatPrint
  , unboundIndentPrint
  , setFromCarrier
  , setMaxWidth
  , removeMaxWidth
  , setIndentAmount
  , setIndentStrategy
  ) where

import Data.SCargot.Parse
import Data.SCargot.Print

{- $intro

The S-Cargot library is a library for parsing and emitting
<https://en.wikipedia.org/wiki/S-expression s-expressions>, designed
to be as flexible as possible. Despite some efforts at
<http://people.csail.mit.edu/rivest/Sexp.txt standardization>,
s-expressions are a general approach to describing a data format
that can very often differ in subtle, incompatible ways: the
s-expressions understood by Common Lisp are different from the
s-expressions understood by Scheme, and even the different
revisions of the Scheme language understand s-expressions in a
slightly different way. To accomodate this, the S-Cargot library
provides a toolbox for defining variations on s-expressions,
complete with the ability to select various comment syntaxes, reader
macros, and atom types.

If all you want is to read some s-expressions and don't care about
the edge cases of the format, or all you want is a new configuration
format, try the "Data.SCargot.Language.Basic" or "Data.SCargot.Language.HaskLike"
modules, which define an s-expression language whose atoms are
plain strings and Haskell literals, respectively.

The S-Cargot library works by specifying values which contain all
the information needed to either parse or print an s-expression.
The actual s-expression structure is parsed as a structure of
<https://en.wikipedia.org/wiki/Cons cons cells> as represented
by the 'SExpr' type, but can alternately be exposed as the
isomorphic 'RichSExpr' type or the less expressive but
easier-to-work-with 'WellFormedSExpr' type. Modules devoted
to each representation type (in "Data.SCargot.Repr.Basic",
"Data.SCargot.Repr.Rich", and "Data.SCargot.Repr.WellFormed")
provide helper functions, lenses, and pattern synonyms to make
creating and processing these values easier.

The details of how to parse a given structure are represented
by building up a 'SExprParser' value, which is defined in
"Data.SCargot.Parse" and re-exported here. A minimal
'SExprParser' defines only how to parse the atoms of the
language; helper functions can define comment syntaxes,
reader macros, and transformations over the parsed structure.

The details of how to print a given structure are represented
by building up a 'SExprPrinter' value, which is defined in
"Data.SCargot.Print" and re-exported here. A minimal
'SExprPrinter' defines only how to print the atoms of the
language; helper functions help with the layout of the
pretty-printed s-expression in terms of how to indent the
surrounding expression.

Other helper modules define useful primitives for building up
s-expression languages: the "Data.SCargot.Common" module provides
parsers for common literals, while the "Data.SCargot.Comments"
module provides parsers for comment syntaxes borrowed from
various other languages.

-}
