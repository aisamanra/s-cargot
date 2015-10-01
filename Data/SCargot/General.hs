{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.General
  ( -- * SExprSpec
    SExprSpec
  , mkSpec
  , convertSpec
  , addReader
  , setComment
    -- * Specific SExprSpec Conversions
  , asRich
  , asWellFormed
  , withQuote
    -- * Using a SExprSpec
  , decode
  , decodeOne
  , encode
  , encodeOne
    -- * Useful Type Aliases
  , Reader
  , Comment
  , Serializer
  ) where

import           Control.Applicative ((<*), (*>), (<*>), (<$>), pure)
import           Control.Monad ((>=>))
import           Data.Char (isAlpha, isDigit, isAlphaNum)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import           Text.Parsec ( (<|>)
                             , (<?>)
                             , char
                             , eof
                             , lookAhead
                             , many1
                             , runParser
                             , skipMany
                             )
import           Text.Parsec.Char (anyChar, space)
import           Text.Parsec.Text (Parser)

import           Data.SCargot.Repr ( SExpr(..)
                                   , RichSExpr
                                   , WellFormedSExpr
                                   , fromRich
                                   , toRich
                                   , fromWellFormed
                                   , toWellFormed
                                   )

type ReaderMacroMap atom = Map Char (Reader atom)

-- | A 'Reader' represents a reader macro: it takes a parser for
--   the S-Expression type and performs as much or as little
--   parsing as it would like, and then returns an S-expression.
type Reader atom = (Parser (SExpr atom) -> Parser (SExpr atom))

-- | A 'Comment' represents any kind of skippable comment. This
--   parser __must__ be able to fail if a comment is not being
--   recognized, and it __must__ not consume any input.
type Comment = Parser ()

-- | A 'Serializer' is any function which can serialize an Atom
--   to 'Text'.
type Serializer atom = atom -> Text

-- | A 'SExprSpec' describes a parser and emitter for a particular
--   variant of S-Expressions. The @atom@ type corresponds to a
--   Haskell type used to represent the atoms, and the @carrier@
--   type corresponds to the parsed S-Expression structure. The
--   'SExprSpec' type is deliberately opaque so that it must be
--   constructed and modified with other helper functions.
data SExprSpec atom carrier = SExprSpec
  { sesPAtom   :: Parser atom
  , sesSAtom   :: Serializer atom
  , readerMap  :: ReaderMacroMap atom
  , comment    :: Maybe Comment
  , postparse  :: SExpr atom -> Either String carrier
  , preserial  :: carrier -> SExpr atom
  }

-- | Create a basic 'SExprSpec' when given a parser and serializer
--   for an atom type. A small minimal 'SExprSpec' that recognizes
--   any alphanumeric sequence as a valid atom looks like:
--
--   > simpleSpec :: SExprSpec Text (SExpr Text)
--   > simpleSpec = mkSpec (pack <$> many1 isAlphaNum) id
mkSpec :: Parser atom -> Serializer atom -> SExprSpec atom (SExpr atom)
mkSpec p s = SExprSpec
  { sesPAtom   = p <?> "atom"
  , sesSAtom   = s
  , readerMap  = M.empty
  , comment    = Nothing
  , postparse  = return
  , preserial  = id
  }

-- | Modify the carrier type for a 'SExprSpec'. This is
--   used internally to convert between various 'SExpr' representations,
--   but could also be used externally to add an extra conversion layer
--   onto a 'SExprSpec'.
--
--   The following defines an S-expression spec that recognizes the
--   language of binary addition trees. It does so by first transforming
--   the internal S-expression representation using 'asWellFormed', and
--   then providing a conversion between the 'WellFormedSExpr' type and
--   an @Expr@ AST. Notice that the below parser uses 'String' as its
--   underlying atom type, instead of some token type.
--
--   > data Expr = Add Expr Expr | Num Int deriving (Eq, Show)
--   >
--   > toExpr :: WellFormedSExpr String -> Either String Expr
--   > toExpr (L [A "+", l, r])     = Add <$> toExpr l <*> toExpr r
--   > toExpr (A c) | all isDigit c = pure (Num (read c))
--   > toExpr c                     = Left ("Invalid expr: " ++ show c)
--   >
--   > fromExpr :: Expr -> WellFormedSExpr String
--   > fromExpr (Add l r) = L [A "+", fromExpr l, fromExpr r]
--   > fromExpr (Num n)   = A (show n)
--   >
--   > mySpec :: SExprSpec String Expr
--   > mySpec = convertSpec toExpr fromExpr $ asWellFormed $ mkSpec parser pack
--   >   where parser = many1 (satisfy isValidChar)
--   >         isValidChar c = isDigit c || c == '+'
convertSpec :: (b -> Either String c) -> (c -> b)
               -> SExprSpec a b -> SExprSpec a c
convertSpec f g spec = spec
  { postparse = postparse spec >=> f
  , preserial = preserial spec . g
  }

-- | Convert the final output representation from the 'SExpr' type
--   to the 'RichSExpr' type.
asRich :: SExprSpec a (SExpr b) -> SExprSpec a (RichSExpr b)
asRich = convertSpec (return . toRich) fromRich

-- | Convert the final output representation from the 'SExpr' type
--   to the 'WellFormedSExpr' type.
asWellFormed :: SExprSpec a (SExpr b) -> SExprSpec a (WellFormedSExpr b)
asWellFormed = convertSpec toWellFormed fromWellFormed

-- | Add the ability to execute some particular reader macro, as
--   defined by its initial character and the 'Parser' which returns
--   the parsed S-Expression. The 'Reader' is passed a 'Parser' which
--   can be recursively called to parse more S-Expressions, and begins
--   parsing after the reader character has been removed from the
--   stream.
--
--   The following defines an S-expression variant that treats
--   @'expr@ as being sugar for @(quote expr)@. Note that this is done
--   already in a more general way by the 'withQuote' function, but
--   it is a good illustration of using reader macros in practice:
--
--   > mySpec :: SExprSpec String (SExpr Text)
--   > mySpec = addReader '\'' reader $ mkSpec (many1 alphaNum) pack
--   >   where reader p = quote <$> p
--   >         quote e  = SCons (SAtom "quote") (SCons e SNil)
addReader :: Char -> Reader a -> SExprSpec a c -> SExprSpec a c
addReader c reader spec = spec
  { readerMap = M.insert c reader (readerMap spec) }

-- | Add the ability to ignore some kind of comment. This gets
--   factored into whitespace parsing, and it's very important that
--   the parser supplied __be able to fail__ (as otherwise it will
--   cause an infinite loop), and also that it __not consume any input__
--   (which may require it to be wrapped in 'try'.)
--
--   The following code defines an S-expression variant that skips
--   C++-style comments, i.e. those which begin with @//@ and last
--   until the end of a line:
--
--   > t :: SExprSpec String (SExpr Text)
--   > t = setComment comm $ mkSpec (many1 alphaNum) pack
--   >   where comm = try (string "//" *> manyTill newline *> pure ())

setComment :: Comment -> SExprSpec a c -> SExprSpec a c
setComment c spec = spec { comment = Just (c <?> "comment") }

-- | Add the ability to understand a quoted S-Expression. In general,
--   many Lisps use @'sexpr@ as sugar for @(quote sexpr)@. This is
--   a convenience function which allows you to easily add quoted
--   expressions to a 'SExprSpec', provided that you supply which
--   atom you want substituted in for the symbol @quote@.
withQuote :: IsString t => SExprSpec t (SExpr t) -> SExprSpec t (SExpr t)
withQuote = addReader '\'' (fmap go)
  where go s  = SCons "quote" (SCons s SNil)

peekChar :: Parser (Maybe Char)
peekChar = Just <$> lookAhead anyChar <|> pure Nothing

parseGenericSExpr ::
  Parser atom  -> ReaderMacroMap atom -> Parser () -> Parser (SExpr atom)
parseGenericSExpr atom reader skip = do
  let sExpr = parseGenericSExpr atom reader skip <?> "s-expr"
  skip
  c <- peekChar
  r <- case c of
    Nothing -> fail "Unexpected end of input"
    Just '(' -> char '(' >> skip >> parseList sExpr skip
    Just (flip M.lookup reader -> Just r) -> anyChar >> r sExpr
    _ -> SAtom `fmap` atom
  skip
  return r

parseList :: Parser (SExpr atom) -> Parser () -> Parser (SExpr atom)
parseList sExpr skip = do
  i <- peekChar
  case i of
    Nothing  -> fail "Unexpected end of input"
    Just ')' -> char ')' >> return SNil
    _        -> do
      car <- sExpr
      skip
      c <- peekChar
      case c of
        Just '.' -> do
          char '.'
          cdr <- sExpr
          skip
          char ')'
          skip
          return (SCons car cdr)
        Just ')' -> do
          char ')'
          skip
          return (SCons car SNil)
        _ -> do
          cdr <- parseList sExpr skip
          return (SCons car cdr)

-- | Given a CommentMap, create the corresponding parser to
--   skip those comments (if they exist).
buildSkip :: Maybe (Parser ()) -> Parser ()
buildSkip Nothing  = skipMany space
buildSkip (Just c) = alternate
  where alternate = skipMany space >> ((c >> alternate) <|> return ())

doParse :: Parser a -> Text -> Either String a
doParse p t = case runParser p () "" t of
  Left err -> Left (show err)
  Right x  -> Right x

-- | Decode a single S-expression. If any trailing input is left after
--   the S-expression (ignoring comments or whitespace) then this
--   will fail: for those cases, use 'decode', which returns a list of
--   all the S-expressions found at the top level.
decodeOne :: SExprSpec atom carrier -> Text -> Either String carrier
decodeOne spec = doParse (parser <* eof) >=> (postparse spec)
  where parser = parseGenericSExpr
                   (sesPAtom spec)
                   (readerMap spec)
                   (buildSkip (comment spec))

-- | Decode several S-expressions according to a given 'SExprSpec'. This
--   will return a list of every S-expression that appears at the top-level
--   of the document.
decode :: SExprSpec atom carrier -> Text -> Either String [carrier]
decode spec =
  doParse (many1 parser <* eof) >=> mapM (postparse spec)
    where parser = parseGenericSExpr
                     (sesPAtom spec)
                     (readerMap spec)
                     (buildSkip (comment spec))

-- | Encode (without newlines) a single S-expression.
encodeSExpr :: SExpr atom -> (atom -> Text) -> Text
encodeSExpr SNil _         = "()"
encodeSExpr (SAtom s) t    = t s
encodeSExpr (SCons x xs) t = go xs (encodeSExpr x t)
  where go (SAtom s) rs = "(" <> rs <> " . " <> t s <> ")"
        go SNil rs      = "(" <> rs <> ")"
        go (SCons x xs) rs = go xs (rs <> " " <> encodeSExpr x t)

-- | Emit an S-Expression in a machine-readable way. This does no
--   pretty-printing or indentation, and produces no comments.
encodeOne :: SExprSpec atom carrier -> carrier -> Text
encodeOne spec c = encodeSExpr (preserial spec c) (sesSAtom spec)

encode :: SExprSpec atom carrier -> [carrier] -> Text
encode spec cs = T.concat (map (encodeOne spec) cs)
