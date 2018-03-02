v0.1.4.0
=======

Features:

* Added `encodeLazy` and `encodeOneLazy` functions to producing lazy
  text

Fixes:

* Added the `Located` type for source location tracking for `atom`
  values (thanks ckoparkar!)
* Added `unconstrainedPrint`, which does not try to restrict a printed
  s-expression to a fixed width but will attempt to indent it in a
  reasonable way nonetheless.

v0.1.3.0
=======

Features:

* Added the `Located` type for source location tracking for `atom`
  values (thanks ckoparkar!)
* Added `unconstrainedPrint`, which does not try to restrict a printed
  s-expression to a fixed width but will attempt to indent it in a
  reasonable way nonetheless.

Fixes:

* Pretty-printing configurations created with `flatPrint` now use a
  _much_ more efficient pretty-printer.
* Internally, pretty-printers use a richer type which improves
  performance somewhat by cutting down on repeated intermediate
  printing, and future work will build on this to make printing even
  more efficient.

v0.1.2.0
=======

* Added `atom` and `mkAtomParser` helper functions for new
  user-defined atom types.
* New parsers for various atom types:
    * Exported parsers for individual Haskell literals, to allow
      building new variations on the `HaskLike` atom type.
    * Added syntaxes for arbitrary-base numeric literals in the style of
      Common Lisp and M4
* Added a suite of basic QuickCheck tests
* Compatibility fix: GHC 7.8 didn't allow type signatures on pattern
  synonyms.


v0.1.1.0
=======

* Strongly considered but did not keep a changelog. …sorry.
