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

There are three built-in representations of S-expression lists: two of them
are isomorphic, as one or the other might be better for processing
S-expression data, and the third represents only a subset of possible
S-expressions.

~~~~
-- cons-based representation
data SExpr atom = SAtom atom | SCons (SExpr atom) (SExpr atom) | SNil

-- list-based representation
data RichSExpr atom
  = RSList [RichSExpr atom]
~~~~
