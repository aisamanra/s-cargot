S-Cargot is a library for parsing and emitting S-expressions, designed
to be flexible, customizable, and extensible. Different uses of
S-expressions often understand subtly different variations on what an
S-expression is. The goal of S-Cargot is to create several reusable
components that can be repurposed to nearly any S-expression variant.

S-Cargot does _not_ aim to be the fastest or most efficient
s-expression library. If you need speed, then it would probably be
best to roll your own [AttoParsec]() parser.
Wherever there's a choice, S-Cargot errs on the side of
maximum flexibility, which means that it should be easy to
plug together components to understand various existing flavors of
s-expressions or to extend it in various ways to accomodate new
flavors.

## What Are S-Expressions?

S-expressions were originally the data representation format in
Lisp implementations, but have found broad uses outside of that as
a data representation and storage format. S-expressions are often
understood as a representation for binary trees with optional values
in the leaf nodes: an empty leaf is represented with empty
parens `()`, a non-empty leaf is represented as the scalar value
it contains (often tokens like `x` or other programming language
literals), and an internal node is represented as `(x . y)` where
`x` and `y` are standing in for other s-expressions. In Lisp
parlance, an internal node is called a _cons cell_, and the first
and second elements inside it are called the _car_ and the _cdr_,
for historical reasons. Non-empty lef nodes are referred to
in the s-cargot library as _atoms_.

Often, s-expressions are used to represent lists, in which case
the list is treated as a right-branching tree with an empty leaf as
the far right child of the tree. S-expression languages have a
shorthand way of representing these lists: instead of writing successsively
nested pairs, as in `(1 . (2 . (3 . ()))`, they allow the sugar
`(1 2 3)`. This is the most common way of writing s-expressions,
even in languages that allow raw cons cells (or "dotted pairs") to
be written.

The s-cargot library refers to expressions where every right-branching
sequence ends in an empty leaf as _well-formed s-expressions_. Note that
any s-expression which can be written without using a dotted pair is
necessarily well-formed.

Unfortunately, while in common use, s-expressions do not have a single
formal standard. They are often defined in an ad-hoc way, which means
that s-expressions used in different contexts will, despite sharing a common
parentheses-delimited structure, differ in various respects. Additionally,
because s-expressions are used as the concrete syntax for languages of
the Lisp family, they often have conveniences (such as comment syntaxes)
and other bits of syntactic sugar (such as _reader macros_, which are
described more fully later) that make parsing them much more complicated.
Even ignoring those features, the _atoms_ recognized by a given
s-expression variation can differ widely.

The s-cargot library was designed to accomodate several different kinds
of s-expression formats, so that an s-expression format can be easily
expressed as a combination of existing features. It includes a few basic
variations on s-expression languages as well as the tools for parsing
and emitting more elaborate s-expressions variations without having to
reimplement the basic plumbing yourself.

## Using the Library

The central way of interacting with the S-Cargot library is by creating
and modifying datatypes which represent specifications for parsing and
printing s-expressions. Each of those types has two type parameters, which
are often called `atom` and `carrier`:

~~~~
                         +------ the type that represents an atom or value
                         |
                         |    +- the Haskell representation of the SExpr itself
                         |    |
parser  :: SExprParser  atom carrier
printer :: SExprPrinter atom carrier
~~~~

Various functions will be provided that modify the carrier type (i.e. the
output type of parsing or input type of serialization) or the language
recognized by the parsing.

## Representing S-expressions

There are three built-in representations of S-expression lists: two of them
are isomorphic, as one or the other might be better for processing
S-expression data in a particular circumstance, and the third represents
only the well-formed subset of possible S-expressions.

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
>>> decode basicParser "(a b)"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
>>> decode (asRich basicParser) "(a b)"
Right [RSList [RSAtom "a",RSAtom "b"]]
>>> decode (asWellFormed basicParser) "(a b)"
Right [WFSList [WFSAtom "a",WFSAtom "b"]]
>>> decode basicParser "(a . b)"
Right [SCons (SAtom "a") (SAtom "b")]
>>> decode (asRich basicParser) "(a . b)"
Right [RSDotted [RSAtom "a"] "b"]
>>> decode (asWellFormed basicParser) "(a . b)"
Left "Found atom in cdr position"
~~~~

These names and patterns can be quite long, so S-Cargot also exports
several pattern synonyms that can be used both as expressions and
in pattern-matches to make working with these types less verbose.
These are each contained in their own module, as their names conflict
with each other, so it's recommended to only import the type that
you plan on working with:

~~~~.haskell
>>> import Data.SCargot.Repr.Basic
>>> A 2 ::: A 3 ::: A 4 ::: Nil
SCons (SAtom 2) (SCons (SAtom 3) (SComs (SAtom 4) SNil))
~~~~

~~~~.haskell
>>> import Data.SCargot.Repr.WellFormed
>>> L [A 1,A 2,A 3]
WFSList [WFSAtom 1,WFSAtom 2,WFSAtom 3]
>>> let sexprSum (L xs) = sum (map sexprSum xs); sexprSum (A n) = n
>>> :t sexprSum
sexprSum :: Num a => WellFormedSExpr a -> a
>>> sexprSum (L [A 2, L [A 3, A 4]])
9
~~~~

If you are using GHC 7.10, several of these will be powerful
bidirectional pattern synonyms that allow both constructing and
pattern-matching on s-expressions in non-trivial ways:

~~~~.haskell
>>> import Data.SCargot.Repr.Basic
>>> L [ A 2, A 3, A 4 ]
SCons (SAtom 2) (SCons (SAtom 3) (SComs (SAtom 4) SNil))
~~~~

## Atom Types

Any type can serve as an underlying atom type provided that it has
a Parsec parser or a serializer (i.e. a way of turning it
into `Text`.) For these examples, I'm going to use a very simple
serializer that is roughly like the one found in `Data.SCargot.Basic`,
which parses symbolic tokens of letters, numbers, and some
punctuation characters. This means that the 'serializer' here
is just the identity function:

~~~~.haskell
parser :: SExprParser Text (SExpr Text)
parser = mkParser (pack <$> many1 (alphaNum <|> oneOf "+-*/!?"))

printer :: SExprPrinter Text (SExpr Text)
printer = flatPrint id
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

myParser :: SExprParser Atom (SExpr Atom)
myParser = mkParser pAtom

myPrinter :: SExprPrinter Atom (SExpr Atom)
myPrinter = flatPrint sAtom
~~~~

We can then use this newly created atom type within an S-expression
for both parsing and serialization:

~~~~.haskell
>>> decode myParser "(foo 1)"
Right [SCons (SAtom (Ident "foo")) (SCons (SAtom (Num 1)) SNil)]
>>> encode mySpec [L [A (Num 0), A (Ident "bar")]]
"(0 bar)"
~~~~

Several common atom types appear in the module
[`Data.SCargot.Common`](https://hackage.haskell.org/package/s-cargot-0.1.0.0/docs/Data-SCargot-Common.html),
including various kinds of identifiers and number literals. The
long-term plan for S-Cargot is to include more and more kinds of
built-in atoms, in order to make putting together an S-Expression
parser even easier. If you have a common syntax for an atom type
that you think should be represented there, please
[suggest it in an issue](https://github.com/aisamanra/s-cargot/issues)!

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
>>> let parser' = setCarrier toExpr (asRich myParser)
>>> :t parser'
SExprParser Atom Expr
>>> decode parser' "(+ 1 2)"
Right [Add (Num 1) (Num 2)]
>>> decode parser' "(0 1 2)"
Left "Unrecognized s-expr"
~~~~

## Comments

By default, an S-expression parser does not include a comment syntax, but
the provided `withLispComments` function will cause it to understand
traditional Lisp line-oriented comments that begin with a semicolon:

~~~~.haskell
>>> decode basicParser "(this ; has a comment\n inside)\n"
Left "(line 1, column 7):\nunexpected \";\"\nexpecting space or atom"
>>> decode (withLispComments basicParser) "(this ; has a comment\n inside)\n"
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
>>> let cppComment = string "//" >> manyTill newline >> return ()
>>> decode (setComment cppComment basicParser) "(a //comment\n  b)\n"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
~~~~

The
[`Data.SCargot.Comments`](https://hackage.haskell.org/package/s-cargot/docs/Data-SCargot-Comments.html)
module defines some helper functions for creating comment syntaxes, so the
`cppComment` parser above could be defined as simply

~~~~.haskell
>>> let cppComment = lineComment "//"
>>> decode (setComment cppComment basicParser) "(a //comment\n  b)\n"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
~~~~

Additionally, a handful of common comment syntaxes are defined in
[`Data.SCargot.Comments`](https://hackage.haskell.org/package/s-cargot/docs/Data-SCargot-Comments.html),
including C-style, Haskell-style, and generic scripting-language-style
comments, so in practice, we could write the above example as

~~~~.haskell
>>> decode (withCLikeLineComments basicParser) "(a //comment\n  b)\n"
Right [SCons (SAtom "a") (SCons (SAtom "b") SNil)]
~~~~

## Reader Macros

A _reader macro_ is a Lisp macro---a function that operates on syntactic
structures---which is invoked during the scanning phase of a Lisp parser. This
allows the _lexical_ syntax of a Lisp to be modified. The most commonly
seen reader macro is the quote, which allows the syntax `'expr` to stand as sugar
for the s-expression `(quote expr)`. The S-Cargot library accomodates
this by keeping a map from characters to Haskell functions that can be used as
readers. There is a special case for the aforementioned quote, but that
could easily be written by hand as

~~~~.haskell
>>> let quote expr = SCons (SAtom "quote") (SCons expr SNil)
>>> let addQuoteReader = addReader '\'' (\ parse -> fmap quoteExpr parse)
>>> decode (addQuoteReader basicParser) "'foo"
Right [SCons (SAtom "quote") (SCons (SAtom "foo") SNil)]
~~~~

A reader macro is passed the parser that invoked it, so that it can
perform recursive calls into the parser, and can return any `SExpr` it would like. It
may also take as much or as little of the remaining parse stream as it
would like; for example, the following reader macro does not bother
parsing anything else and merely returns a new token:

~~~~.haskell
>>> let qmReader = addReader '?' (\ _ -> pure (SAtom "huh"))
>>> decode (qmReader basicParser) "(?1 2)"
Right [SCons (SAtom "huh") (SCons (SAtom "1") (SCons (SAtom "2") SNil))]
~~~~

Reader macros in S-Cargot can be used to define bits of Lisp
syntax that are not typically considered the purview of S-expression
parsers. For example, to allow square brackets as a subsitute for
proper lists, we could define a reader macro that is indicated by the
`[` character and repeatedly calls the parser until a `]` character
is reached:

~~~~.haskell
>>> let vec p = (char ']' *> pure SNil) <|> (SCons <$> p <*> vec p)
>>> let withVecReader = addReader '[' vec
>>> decode (asRich (withVecReader basicParser)) "(1 [2 3])"
Right [RSList [RSAtom "1",RSList [RSAtom "2",RSAtom "3"]]]
~~~~

## Pretty-Printing and Indentation

The s-cargot library also includes a simple but often adequate
pretty-printing system for s-expressions. A printer that prints a
single-line s-expression is created with `flatPrint`:

~~~~.haskell
>>> let printer = flatPrint id
>>> :t printer
SExprPrinter Text (SCargot Text)
>>> Text.putStrLn $ encode printer [L [A "foo", A "bar"]]
(foo bar)
~~~~

A printer that tries to pretty-print an s-expression to fit
attractively within an 80-character limit can be created with
`basicPrint`:

~~~~.haskell
>>> let printer = basicPrint id
>>> let sentence = "this stupendously preposterously supercalifragilisticexpialidociously long s-expression"
>>> let longSexpr = L [A word | word <- Text.words sentence ]
>>> Text.putStrLn $ encodeOne printer longSexpr
(this
  stupendously
  preposterously
  supercalifragilisticexpialidociously
  long
  s-expression)
~~~~

A printer created with `basicPrint` will "swing" things that are too
long onto the subsequent line, indenting it a fixed number of spaces.
We can modify the number of spaces with `setIndentAmount`:

~~~~.haskell
>>> let printer = setIndentAmount 4 (basicPrint id)
>>> Text.putStrLn $ encodeOne printer longSexpr
(this
    stupendously
    preposterously
    supercalifragilisticexpialidociously
    long
    s-expression)
~~~~

We can also modify what counts as the 'maximum width', which for a
`basicPrint` printer is 80 by default:

~~~~.haskell
>>> let printer = setMaxWidth 8 (basicPrint id)
>>> Text.putStrLn $ encodeOne printer (L [A "one", A "two", A "three"])
(one
  two
  three)
~~~~

Or remove the maximum, which will put the whole s-expression onto one
line, regardless of its length:

~~~~.haskell
>>> let printer = removeMaxWidth (basicPrint id)
>>> Text.putStrLn $ encodeOne printer longSexpr
(this stupendously preposterously supercalifragilisticexpialidociously long s-expression)
~~~~

We can also specify an _indentation strategy_, which decides how to
indent subsequent expressions based on the head of a given
expression. The default is to always "swing" subsequent expressions
to the next line, but we could also specify the `Align` constructor, which
will print the first two expressions on the same line and then any subsequent
expressions horizontally aligned with the second one, like so:

~~~~.haskell
>>> let printer = setIndentStrategy (\ _ -> Align) (setMaxWidth 8 (basicPrint id))
>>> Text.putStrLn $ encodeOne printer (L [A "one", A "two", A "three", A "four"])
(one two
     three
     four)
~~~~

Or we could choose to keep some number of expressions on the same line and afterwards
swing the subsequent ones:

~~~~.haskell
>>> let printer = setIndentStrategy (\ _ -> SwingAfter 1) (setMaxWidth 8 (basicPrint id))
>>> Text.putStrLn $ encodeOne printer (L [A "one", A "two", A "three", A "four"])
(one two
  three
  four)
~~~~

For lots of situations, we might want to choose a different indentation strategy based
on the first expression within a proper list: for example, Common Lisp source code is often
formatted so that, following a `defun` token, we have the function name and arguments
on the same line, and then the body of the function indented some amount subsequently.
We can express an approximation of that strategy like this:

~~~~.haskell
>>> let strategy (A ident) | "def" `Text.isPrefixOf` ident = SwingAfter 2; strategy _ = Align
>>> let printer = setIndentStrategy strategy (setMaxWidth 20 (basicPrint id))
>>> let fact = L [A "defun", A "fact", L [A "x"], L [A "product", L [A "range", A "1", A "x"]]]
>>> Text.putStrLn $ encodeOne printer fact
(defun fact (x)
  (product (range 1 x)))
>>> let app = L [A "apply", L [A "lambda", L [A "y"], L [A "fact", A "y"]], L [A "+", A "2", A "3"]]
(apply (lambda (y) (fact y)
       (+ 2 3))
~~~~

## Putting It All Together

Here is a final example which implements a limited arithmetic language
with Haskell-style line comments and a special reader macro to understand hex
literals:

~~~~.haskell
{-# LANGUAGE OverloadedStrings #-}

module SCargotExample where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.SCargot
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack)
import Numeric (readHex)
import Text.Parsec (anyChar, char, digit, many1, manyTill, newline, satisfy, string)
import Text.Parsec.Text (Parser)

-- Our operators are going to represent addition, subtraction, or
-- multiplication
data Op = Add | Sub | Mul deriving (Eq, Show)

-- The atoms of our language are either one of the aforementioned
-- operators, or positive integers
data Atom = AOp Op | ANum Int deriving (Eq, Show)

-- Once parsed, our language will consist of the applications of
-- binary operators with literal integers at the leaves
data Expr = EOp Op Expr Expr | ENum Int deriving (Eq, Show)

-- Conversions to and from our Expr type
toExpr :: SExpr Atom -> Either String Expr
toExpr (A (AOp op) ::: l ::: r ::: Nil) = EOp op <$> toExpr l <*> toExpr r
toExpr (A (ANum n)) = pure (ENum n)
toExpr sexpr = Left ("Unable to parse expression: " ++ show sexpr)

fromExpr :: Expr -> SExpr Atom
fromExpr (EOp op l r) = A (AOp op) ::: fromExpr l ::: fromExpr r ::: Nil
fromExpr (ENum n)     = A (ANum n) ::: Nil

-- Parser and serializer for our Atom type
pAtom :: Parser Atom
pAtom = ((ANum . read) <$> many1 digit)
     <|> (char '+' *> pure (AOp Add))
     <|> (char '-' *> pure (AOp Sub))
     <|> (char '*' *> pure (AOp Mul))

sAtom :: Atom -> Text
sAtom (AOp Add) = "+"
sAtom (AOp Sub) = "-"
sAtom (AOp Mul) = "*"
sAtom (ANum n)  = pack (show n)

-- Our comment syntax is going to be Haskell-like:
hsComment :: Parser ()
hsComment = string "--" >> manyTill anyChar newline >> return ()

-- Our custom reader macro: grab the parse stream and read a
-- hexadecimal number from it:
hexReader :: Reader Atom
hexReader _ = (A . ANum . rd) <$> many1 (satisfy isHexDigit)
  where isHexDigit c = isDigit c || c `elem` hexChars
        rd = fst . head . readHex
        hexChars :: String
        hexChars = "AaBbCcDdEeFf"

-- Our final s-expression parser and printer:
myLangParser :: SExprParser Atom Expr
myLangParser
  = setComment hsComment        -- set comment syntax to be Haskell-style
  $ addReader '#' hexReader     -- add hex reader
  $ setCarrier toExpr           -- convert final repr to Expr
  $ mkParser pAtom              -- create spec with Atom type

mkLangPrinter :: SExprPrinter Atom Expr
mkLangPrinter
  = setFromCarrier fromExpr
  $ setIndentStrategy (const Align)
  $ basicPrint sAtom

>>> decode myLangParser "(+ (* 2 20) 10) (* 10 10)"
[EOp Add (EOp Mul (ENum 2) (ENum 20)) (ENum 10),EOp Mul (ENum 10) (ENum 10)]
~~~~

Keep in mind that you often won't need to write all this by hand,
as you can often use a variety of built-in atom types, reader
macros, comment types, and representations, but it's a useful
illustration of all the options that are available to you should
you need them!
