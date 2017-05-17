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
