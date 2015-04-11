S-Cargot is a library for parsing and emitting S-expressions, designed
to be flexible, customizable, and extensible. Different uses of
S-expressions often understand subtly different variations on what an
S-expression is. The goal of S-Cargot is to create several reusable
components that can be repurposed to nearly any S-expression variant.

Additionally, S-Cargot uses these to include out-of-the-box parsing and
processing for several existing variations on S-expressions, including
Common Lisp (**in progresss**), Scheme (**in progress**), the
[Rivest internet-draft](http://people.csail.mit.edu/rivest/Sexp.txt)
(**in progress**), and Clojure (**in progress**).

S-Cargot does _not_ aim to be the fastest or most efficient
s-expression library. If you need speed, then it would probably be
best to roll your own [AttoParsec]() parser.
Wherever there's a choice, S-Cargot errs on the side of
maximum flexibility, which means that it should be easy to
plug together components to understand various existing flavors of
s-expressions or to extend it in various ways to accomodate new
flavors.

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
S-expression data in a particular circumstance, and the third represents
only a subset of possible S-expressions.

~~~~.haskell
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

The `WellFormedSExpr` representation should be structurally
identical to the `RichSExpr` representation in all cases where
no improper lists appear in the source. Both of those are
often more convenient than writing multiple nested `SCons`
constructors in Haskell.

Functions for converting back and forth between
representations are provided, but you can also modify a
`SExprSpec` to parse to or serialize from a particular
representation using the `asRich` and `asWellFormed`
functions.

~~~~.haskell
> decode spec "(a b)"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
> decode (asRich spec) "(a b)"
Right [RSList [RSAtom "a",RSAtom "b"]]
> decode (asWellFormed spec) "(a b)"
Right [WFSList [WFSAtom "a",WFSAtom "b"]]
> decode spec "(a . b)"
Right [SCons (SAtom "a") (SAtom "b")]
> decode (asRich spec) "(a . b)"
Right [RSDotted [RSAtom "a"] "b"]
> decode (asWellFormed spec) "(a . b)"
Left "Found atom in cdr position"
~~~~

These names and patterns can be quite long, so S-Cargot also exports
several pattern synonyms that can be used both as expressions and
in pattern-matches to make working with these types less verbose.
These are each contained in their own module, as their names conflict
with each other, so it's recommended to only import the type that
you plan on working with:

~~~~.haskell
> A 2 ::: A 3 ::: A 4 ::: Nil
SCons (SCons (SCons (SAtom 2) (SAtom 3)) (SAtom 4)) SNil
~~~~

~~~~.haskell
> L [A 1,A 2,A 3]
WFSList [WFSAtom 1,WFSAtom 2,WFSAtom 3]
> let sexprSum (L xs) = sum (map sexprSum xs); sexprSum (A n) = n
> :t sexprSum
sexprSum :: Num a => WellFormedSExpr a -> a
> sexprSum (L [A 2, L [A 3, A 4]])
9
~~~~

## Atom Types

Any type can serve as an underlying atom type provided that it has
an Parsec parser and a serializer (i.e. a way of turning it
into `Text`.) For these examples, I'm going to use a very simple
serializer that is roughly like the one found in `Data.SCargot.Basic`,
which parses symbolic tokens of letters, numbers, and some
punctuation characters. This means that the 'serializer' here
is just the identity function:

~~~~.haskell
spec :: SExprSpec Text (SExpr Text)
spec = mkSpec (pack <$> many1 (alphaNum <|> oneOf "+-*/!?")) id
~~~~

A more elaborate atom type would distinguish between different
varieties of token, so a small example (that understands just
identifiers and numbers) is

~~~~.haskell
import Data.Text (Text, pack)

data Atom = Ident Text | Num Int deriving (Eq, Show)

pAtom :: Parser Atom
pAtom =  ((Num . read) <$> many1 digit)
     <|> (Ident . pack) <$> takeWhile1 isAlpha)

sAtom :: Atom -> Text
sAtom (Ident t) = t
sAtom (Num n)   = pack (show n)

mySpec :: SExprSpec Atom (SExpr Atom)
mySpec = mkSpec pAtom sAtom
~~~~

We can then use this newly created atom type within an S-expression
for both parsing and serialization:

~~~~.haskell
> decode mySpec "(foo 1)"
Right [SCons (SAtom (Ident "foo")) (SCons (SAtom (Num 1)) SNil)]
> encode mySpec [SCons (SAtom (Num 0)) SNil]
"(0)"
~~~~

## Carrier Types

As pointed out above, there are three different carrier types that are
used to represent S-expressions by the library, but you can use any
type as a carrier type for a spec. This is particularly useful when
you want to parse into your own custom tree-like type. For example, if
we wanted to parse a small S-expression-based arithmetic language, we
could define a data type and transformations from and to an S-expression
type:

~~~~.haskell
import           Data.Char (isDigit)
import           Data.SCargot.General
import           Data.Text (Text)
import qualified Data.Text as T


data Expr = Add Expr Expr | Num Int deriving (Eq, Show)

toExpr :: RichSExpr Text -> Either String Expr
toExpr (L [A "+", l, r]) = Add <$> toExpr l <*> toExpr r
toExpr (A c)
  | T.all isDigit c = pure (Num (read (T.unpack c)))
  | otherwise       = Left "Non-numeric token as argument"
toExpr _ = Left "Unrecognized s-expr"

fromExpr :: Expr -> RichSExpr Text
fromExpr (Add x y) = L [A "+", fromExpr x, fromExpr y]
fromExpr (Num n)   = A (T.pack (show n))
~~~~

then we could use the `convertSpec` function to add this directly to
the `SExprSpec`:

~~~~.haskell
> decode (convertSpec toExpr fromExpr (asRich spec)) "(+ 1 2)"
Right [Add (Num 1) (Num 2)]
> decode (convertSpec toExpr fromExpr (asRich spec)) "(0 1 2)"
Left "Unrecognized s-expr"
~~~~

## Comments

By default, an S-expression spec does not include a comment syntax, but
the provided `withLispComments` function will cause it to understand
traditional Lisp line-oriented comments that begin with a semicolon:

~~~~.haskell
> decode spec "(this ; has a comment\n inside)\n"
Left "(line 1, column 7):\nunexpected \";\"\nexpecting space or atom"
> decode (withLispComments spec) "(this ; has a comment\n inside)\n"
Right [SCons (SAtom "this") (SCons (SAtom "inside") SNil)]
~~~~

Additionally, you can provide your own comment syntax in the form of an
Parsec parser. Any Parsec parser can be used, so long as it meets
the following criteria:
- it is capable of failing (as is called until SCargot believes that there
are no more comments)
- it does not consume any input in the case of failure, which may involve
wrapping the parser in a call to `try`

For example, the following adds C++-style comments to an S-expression format:

~~~~.haskell
> let cppComment = string "//" >> manyTill newline >> return ()
> decode (setComment cppComment spec) "(a //comment\n  b)\n"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
~~~~

## Reader Macros

A _reader macro_ is a Lisp macro which is invoked during read time. This
allows the _lexical_ syntax of a Lisp to be modified. The most commonly
seen reader macro is the quote, which allows the syntax `'expr` to stand
in for the s-expression `(quote expr)`. The S-Cargot library accomodates
this by keeping a map of characters to Parsec parsers that can be used as
readers. There is a special case for the aforementioned quote, but that
could easily be written by hand as

~~~~.haskell
> let quoteExpr c = SCons (SAtom "quote") (SCons c SNil)
> let withQuote = addReader '\'' (\ p -> fmap quoteExpr p)
> decode (withQuote mySpec) "'foo"
Right [SCons (SAtom "quote") (SCons (SAtom "foo") SNil)]
~~~~

A reader macro is passed the parser that invoked it, so that it can
perform recursive calls, and can return any `SExpr` it would like. It
may also take as much or as little of the remaining parse stream as it
would like; for example, the following reader macro does not bother
parsing anything else and merely returns a new token:

~~~~.haskell
> let qmReader = addReader '?' (\ _ -> pure (SAtom "huh"))
> decode (qmReader mySpec) "(?1 2)"
Right [SCons (SAtom "huh") (SCons (SAtom "1") (SCons (SAtom "2") SNil))]
~~~~

Reader macros in S-Cargot can be used to define common bits of Lisp
syntax that are not typically considered the purview of S-expression
parsers. For example, to allow square brackets as a subsitute for
proper lists, we could define a reader macro that is initialized by the
`[` character and repeatedly calls the parser until a `]` character
is reached:

~~~~.haskell
> let pVec p = (char ']' *> pure SNil) <|> (SCons <$> p <*> pVec p)
> let vec = addReader '[' pVec
> decode (asRich (vec mySpec)) "(1 [2 3])"
Right [RSList [RSAtom "1",RSList [RSAtom "2",RSAtom "3"]]]
~~~~

## Putting It All Together

Here is a final example which implements a limited arithmetic language
with Haskell-style line comments and a special reader to understand hex
literals:

~~~~.haskell
data Op = Add | Sub | Mul deriving (Eq, Show)
data Atom = AOp Op | ANum Int deriving (Eq, Show)
data Expr = EOp Op Expr Expr | ENum Int deriving (Eq, Show)

-- Conversions for our Expr type
toExpr :: SExpr Atom -> Either String Expr
toExpr (A (AOp op) ::: l ::: r ::: Nil) = EOp op <$> l <*> r
toExpr (A (ANum n)) = pure (ENum n)
toExpr sexpr = Left ("Invalid parse: " ++ show sexpr)

fromExpr :: Expr -> SExpr Atom
fromExpr (EOp op l r) = A (AOp op) ::: fromExpr l ::: fromExpr r ::: Nil
fromExpr (ENum n)     = ANum n

-- Parser and serializer for our Atom type
pAtom :: Parser Atom
pAtom = ((ANum . read . T.unpack) <$> many1 isDigit)
     <|> (char "+" *> pure (AOp Add))
     <|> (char "-" *> pure (AOp Sub))
     <|> (char "*" *> pure (AOp Mul))

sAtom :: Atom -> Text
sAtom (AOp Add) = "+"
sAtom (AOp Sub) = "-"
sAtom (AOp Mul) = "*"
sAtom (ANum n)  = T.pack (show n)

-- Our comment syntax
hsComment :: Parser ()
hsComment = string "--" >> manyTill newline >> return ()

-- Our custom reader macro
hexReader :: Reader Atom
hexReader _ = (Num . readHex . T.unpack) <$> takeWhile1 isHexDigit
  where isHexDigit c = isDigit c || c `elem` "AaBbCcDdEeFf"
        rd = readHex . head . fst

-- Our final s-expression family
myLangSpec :: SExprSpec Atom Expr
myLangSpec
  = setComment hsComment        -- set comment syntax to be Haskell-style
  $ addReader '#' hexReader     -- add hex reader
  $ convertSpec toExpr fromExpr -- convert final repr to Expr
  $ mkSpec pAtom sAtom          -- create spec with Atom type
~~~~

Keep in mind that you often won't need to write all this by hand,
as you can often use a variety of built-in atom types, reader
macros, comment types, and representations, but it's a useful
illustration of all the options that are available to you should
you need them!
