S-Cargot is a library for parsing and emitting S-expressions, designed
to be flexible, customizable, and extensible. Different uses of
S-expressions often understand subtly different variations on what an
S-expression is. The goal of S-Cargot is to create as many reusable
components that can be repurposed to nearly any S-expression variant.

Additionally, S-Cargot uses these to include out-of-the-box parsing and
processing for several existing variations on S-expressions, including
Common Lisp (**in progresss**), Scheme (**in progress**), the
[Rivest internet-draft](http://people.csail.mit.edu/rivest/Sexp.txt)
(**in progress**), and Clojure (**in progress**).

The central way of interacting with the S-Cargot library is by creating
and modifying a _spec_, which is a value that represents a given
family of S-expressions. A _spec_, which is of type `SExprSpec`,
contains the information necessary to implement reader macros, arbitrary
kinds of comments, and various processing steps. A `SExprSpec` has two
type parameters:

~~~~
                      +------ the type that represents a SExpr atom
                      |
                      |    +- the Haskell representation of the SExpr value
                      |    |
someSpec :: SExprSpec atom carrier
~~~~

Various functions will be provided that modify the carrier type (i.e. the
output type of parsing or input type of serialization) or the language
recognized by the parsing. Examples will be shown below.

## Representing S-expressions

There are three built-in representations of S-expression lists: two of them
are isomorphic, as one or the other might be better for processing
S-expression data, and the third represents only a subset of possible
S-expressions.

~~~~
-- cons-based representation
data SExpr atom
  = SCons (SExpr atom) (SExpr atom)
  | SNil
  | SAtom atom

-- list-based representation
data RichSExpr atom
  = RSList [RichSExpr atom]
  | RSDotList [RichSExpr atom] atom
  | RSAtom atom

-- well-formed representation
data WellFormedSExpr atom
  = WFSList [WellFormedSExpr atom]
  | WFSAtom atom
~~~~

In the above, an `RSList [a, b, c]` and a
`WFList [a, b, c]` both correspond to the structure
`SCons a (SCons b (SCons d SNil))`, which corresponds to an
S-expression which can be written as
`(a b c)` or as `(a . (b . (c . ())))`. A `RSDotList`
corresponds to an sequence of conses that does not terminate
in an empty list, e.g. `RSDotList [a, b] c` corresponds to
`SCons a (SCons b (SAtom c))`, which in turn corresponds to
a structure like `(a b . c)` or `(a . (b . c))`.

Functions for converting back and forth between
representations are provided, but you can also modify a
`SExprSpec` to parse to or serialize from a particular
representation using the `asRich` and `asWellFormed`
functions.

~~~~
*Data.SCargot.General> decode spec "(a b c)"
Right [SCons (SAtom "a") (SCons (SAtom "b") (SCons (SAtom "c") SNil))]
*Data.SCargot.General> decode (asRich spec) "(a b c)"
Right [RSList [RSAtom "a",RSAtom "b",RSAtom "c"]]
*Data.SCargot.General> decode (asWellFormed spec) "(a b c)"
Right [WFSList [WFSAtom "a",WFSAtom "b",WFSAtom "c"]]
*Data.SCargot.General> decode spec "(a . b)"
Right [SCons (SAtom "a") (SAtom "b")]
*Data.SCargot.General> decode (asRich spec) "(a . b)"
Right [RSDotted [RSAtom "a"] "b"]
*Data.SCargot.General> decode (asWellFormed spec) "(a . b)"
Left "Found atom in cdr position"
~~~~

## Atom Types

Any type can serve as an underlying atom type provided that it has
an AttoParsec parser and a serializer (i.e. a way of turning it
into `Text`.) For these examples, I'm going to use a very simple
serializer that is roughly like the one found in `Data.SCargot.Basic`,
which parses symbolic tokens of letters, numbers, and some
punctuation characters. This means that the 'serializer' here
is just the identity function:

~~~~.haskell
spec :: SExprSpec Text (SExpr Text)
spec = mkSpec (takeWhile1 (\ c -> isAlphaNum c || c `elem` "+-*/!?")) id
~~~~

A more elaborate atom type would distinguish between different
varieties of token, so a small example (that understands just
identifiers and numbers) is

~~~~.haskell
import           Data.Char (isDigit, isAlpha)
import           Data.Text (Text)
import qualified Data.Text as T

data Atom = Ident Text | Num Int deriving (Eq, Show)

pAtom :: Parser Atom
pAtom =  ((Num . read . T.unpack) <$> takeWhile1 isDigit)
     <|> (Ident <$> takeWhile1 isAlpha)

sAtom :: Atom -> Text
sAtom (Ident t) = t
sAtom (Num n)   = T.pack (show n)

mySpec :: SExprSpec Atom (SExpr Atom)
mySpec = mkSpec pAtom sAtom
~~~~

We can then use this newly created atom type within an S-expression
for both parsing and serialization:

~~~~
*Data.SCargot.General T> decode mySpec "(foo 1)"
Right [SCons (SAtom (Ident "foo")) (SCons (SAtom (Num 1)) SNil)]
*Data.SCargot.General T> encode mySpec [SCons (SAtom (Num 0)) SNil]
"(0)"
~~~~

## Carrier Types

As pointed out above, there are three different carrier types that are
used to represent S-expressions by the library, but you can use any
type as a carrier type for a spec. This is particularly useful when
you want to do your own parsing. For example, if we wanted to parse
a small S-expression-based arithmetic language, we could define a
data type and transformations from and to an S-expression type:

~~~~.haskell
import           Data.Char (isDigit)
import           Data.Text (Text)
import qualified Data.Text as T

data Expr = Add Expr Expr | Num Int deriving (Eq, Show)

toExpr :: RichSExpr Text -> Either String Expr
toExpr (RSList [RSAtom "+", l, r]) = Add <$> toExpr l <*> toExpr r
toExpr (RSAtom c)
  | T.all isDigit c = pure (Num (read (T.unpack c)))
  | otherwise       = Left "Non-numeric token as argument"
toExpr _ = "Unrecognized s-expr"

fromExpr :: Expr -> RichSExpr Text
fromExpr (Add x y) = RSList [RSAtom "+", fromExpr x, fromExpr y]
fromExpr (Num n) = RSAtom (T.pack (show n))
~~~~

then we could use the `convertSpec` function to add this directly to
the `SExprSpec`:

~~~~
*Data.SCargot.General T> decode (convertSpec toExpr fromExpr (asRich spec)) "(+ 1 2)"
Right [Add (Num 1) (Num 2)]
*Data.SCargot.General T> decode (convertSpec toExpr fromExpr (asRich spec)) "(0 1 2)"
Left "Unrecognized s-expr"
~~~~

## Comments

By default, an S-expression spec does not include a comment syntax, but
the provided `withSemicolonComments` function will cause it to understand
traditional Lisp line-oriented comments that begin with a semicolon:

~~~~
*Data.SCargot.General> decode spec "(this ; has a comment\n inside)\n"
Left "Failed reading: takeWhile1"
*Data.SCargot.General> decode (withSemicolonComments spec) "(this ; has a comment\n inside)\n"
Right [SCons (SAtom "this") (SCons (SAtom "inside") SNil)]
~~~~

Additionally, you can provide your own comment syntax in the form of an
AttoParsec parser. Any AttoParsec parser can be used, so long as it meets
the following criteria:
- it is capable of failing (as is called until SCargot believes that there
are no more comments)
- it does not consume any input in the case of failure, which may involve
wrapping the parser in a call to `try`

For example, the following adds C++-style comments to an S-expression format:

~~~~
*Data.SCargot.General> let cppComment = string "//" >> takeWhile (/= '\n') >> return ()
*Data.SCargot.General> decode (setComment cppComment spec) "(a //comment\n  b)\n"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
~~~~

## Reader Macros

A _reader macro_ is a Lisp macro which is invoked during read time. This
allows the _lexical_ syntax of a Lisp to be modified. The most commonly
seen reader macro is the quote, which allows the syntax `'expr` to stand
in for the s-expression `(quote expr)`. The S-Cargot library enables this
by keeping a map of characters to AttoParsec parsers that can be used as
readers. There is a special case for the aforementioned quote, but that
could easily be written by hand as

~~~~
*Data.SCargot.General> let doQuote c = SCons (SAtom "quote") (SCons c SNil)
*Data.SCargot.General> let qReader = addReader '\'' (\ p -> fmap doQuote p)
*Data.SCargot.General> decode (qReader mySpec) "'foo"
Right [SCons (SAtom "quote") (SCons (SAtom "foo") SNil)]
~~~~

A reader macro is passed the parser that invoked it, so that it can
perform recursive calls, and can return any `SExpr` it would like. It
may also take as much or as little of the remaining parse stream as it
would like; for example, the following reader macro does not bother
parsing anything else and merely returns a new token:

~~~~
*Data.SCargot.General> let qmReader = addReader '?' (\ _ -> pure (SAtom "huh"))
*Data.SCargot.General> decode (qmReader mySpec) "(?1 2)"
Right [SCons (SAtom "huh") (SCons (SAtom "1") (SCons (SAtom "2") SNil))]
~~~~

Reader macros in S-Cargot can be used to define common bits of Lisp
syntax that are not typically considered the purview of S-expression
parsers. For example, to allow square brackets as a subsitute for
proper lists, we could define a reader macro that is initialized by the
`[` character and repeatedly calls the parser until a `]` character
is reached:

~~~~
*Data.SCargot.General> let pVec p = (char ']' *> pure SNil) <|> (SCons <$> p <*> pVec p)
*Data.SCargot.General> let vec = addReader '[' pVec
*Data.SCargot.General> decode (asRich (vec mySpec)) "(1 [2 3])"
Right [RSList [RSAtom "1",RSList [RSAtom "2",RSAtom "3"]]]
~~~~
