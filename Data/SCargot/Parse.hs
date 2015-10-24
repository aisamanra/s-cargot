{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.Parse
  ( -- * Parsing
    decode
  , decodeOne
    -- * Parsing Control
  , SExprParser
  , Reader
  , Comment
  , mkParser
  , setCarrier
  , addReader
  , setComment
    -- * Specific SExprParser Conversions
  , asRich
  , asWellFormed
  , withQuote
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), pure)
#endif
import           Control.Monad ((>=>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.String (IsString)
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
                                   , toRich
                                   , toWellFormed
                                   )

type ReaderMacroMap atom = Map Char (Reader atom)

-- | A 'Reader' represents a reader macro: it takes a parser for
--   the S-Expression type and performs as much or as little
--   parsing as it would like, and then returns an S-expression.
type Reader atom = (Parser (SExpr atom) -> Parser (SExpr atom))

-- | A 'Comment' represents any kind of skippable comment. This
--   parser __must__ be able to fail if a comment is not being
--   recognized, and it __must__ not consume any input in case
--   of failure.
type Comment = Parser ()

-- | A 'SExprParser' describes a parser for a particular value
--   that has been serialized as an s-expression. The @atom@ parameter
--   corresponds to a Haskell type used to represent the atoms,
--   and the @carrier@ parameter corresponds to the parsed S-Expression
--   structure.
data SExprParser atom carrier = SExprParser
  { sesPAtom   :: Parser atom
  , readerMap  :: ReaderMacroMap atom
  , comment    :: Maybe Comment
  , postparse  :: SExpr atom -> Either String carrier
  }

-- | Create a basic 'SExprParser' when given a parser
--   for an atom type.
--
--   >>> import Text.Parsec (alphaNum, many1)
--   >>> let parser = mkParser (many1 alphaNum)
--   >>> decode parser "(ele phant)"
--   Right [SCons (SAtom "ele") (SCons (SAtom "phant") SNil)]
mkParser :: Parser atom -> SExprParser atom (SExpr atom)
mkParser parser = SExprParser
  { sesPAtom   = parser
  , readerMap  = M.empty
  , comment    = Nothing
  , postparse  = return
  }

-- | Modify the carrier type for a 'SExprParser'. This is
--   used internally to convert between various 'SExpr' representations,
--   but could also be used externally to add an extra conversion layer
--   onto a 'SExprParser'.
--
-- >>> import Text.Parsec (alphaNum, many1)
-- >>> import Data.SCargot.Repr (toRich)
-- >>> let parser = setCarrier (return . toRich) (mkParser (many1 alphaNum))
-- >>> decode parser "(ele phant)"
-- Right [RSlist [RSAtom "ele",RSAtom "phant"]]
setCarrier :: (b -> Either String c) -> SExprParser a b -> SExprParser a c
setCarrier f spec = spec { postparse = postparse spec >=> f }

-- | Convert the final output representation from the 'SExpr' type
--   to the 'RichSExpr' type.
--
-- >>> import Text.Parsec (alphaNum, many1)
-- >>> let parser = asRich (mkParser (many1 alphaNum))
-- >>> decode parser "(ele phant)"
-- Right [RSlist [RSAtom "ele",RSAtom "phant"]]
asRich :: SExprParser a (SExpr b) -> SExprParser a (RichSExpr b)
asRich = setCarrier (return . toRich)

-- | Convert the final output representation from the 'SExpr' type
--   to the 'WellFormedSExpr' type.
--
-- >>> import Text.Parsec (alphaNum, many1)
-- >>> let parser = asWellFormed (mkParser (many1 alphaNum))
-- >>> decode parser "(ele phant)"
-- Right [WFSList [WFSAtom "ele",WFSAtom "phant"]]
asWellFormed :: SExprParser a (SExpr b) -> SExprParser a (WellFormedSExpr b)
asWellFormed = setCarrier toWellFormed

-- | Add the ability to execute some particular reader macro, as
--   defined by its initial character and the 'Parser' which returns
--   the parsed S-Expression. The 'Reader' is passed a 'Parser' which
--   can be recursively called to parse more S-Expressions, and begins
--   parsing after the reader character has been removed from the
--   stream.
--
-- >>> import Text.Parsec (alphaNum, char, many1)
-- >>> let vecReader p = (char ']' *> pure SNil) <|> (SCons <$> p <*> vecReader p)
-- >>> let parser = addReader '[' vecReader (mkParser (many1 alphaNum))
-- >>> decode parser "(an [ele phant])"
-- Right [SCons (SAtom "an") (SCons (SCons (SAtom "ele") (SCons (SAtom "phant") SNil)) SNil)]

addReader :: Char -> Reader a -> SExprParser a c -> SExprParser a c
addReader c reader spec = spec
  { readerMap = M.insert c reader (readerMap spec) }

-- | Add the ability to ignore some kind of comment. This gets
--   factored into whitespace parsing, and it's very important that
--   the parser supplied __be able to fail__ (as otherwise it will
--   cause an infinite loop), and also that it __not consume any input__
--   (which may require it to be wrapped in 'try'.)
--
-- >>> import Text.Parsec (alphaNum, anyChar, manyTill, many1, string)
-- >>> let comment = string "//" *> manyTill anyChar newline *> pure ()
-- >>> let parser = setComment comment (mkParser (many1 alphaNum))
-- >>> decode parser "(ele //a comment\n  phant)"
-- Right [SCons (SAtom "ele") (SCons (SAtom "phant") SNil)]

setComment :: Comment -> SExprParser a c -> SExprParser a c
setComment c spec = spec { comment = Just (c <?> "comment") }

-- | Add the ability to understand a quoted S-Expression.
--   Many Lisps use @'sexpr@ as sugar for @(quote sexpr)@. This
--   assumes that the underlying atom type implements the "IsString"
--   class, and will create the @quote@ atom using @fromString "quote"@.
--
-- >>> import Text.Parsec (alphaNum, many1)
-- >>> let parser = withQuote (mkParser (many1 alphaNum))
-- >>> decode parser "'elephant"
-- Right [SCons (SAtom "quote") (SCons (SAtom "foo") SNil)]
withQuote :: IsString t => SExprParser t (SExpr t) -> SExprParser t (SExpr t)
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
          _ <- char '.'
          cdr <- sExpr
          skip
          _ <- char ')'
          skip
          return (SCons car cdr)
        Just ')' -> do
          _ <- char ')'
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
decodeOne :: SExprParser atom carrier -> Text -> Either String carrier
decodeOne spec = doParse (parser <* eof) >=> (postparse spec)
  where parser = parseGenericSExpr
                   (sesPAtom spec)
                   (readerMap spec)
                   (buildSkip (comment spec))

-- | Decode several S-expressions according to a given 'SExprParser'. This
--   will return a list of every S-expression that appears at the top-level
--   of the document.
decode :: SExprParser atom carrier -> Text -> Either String [carrier]
decode spec =
  doParse (many1 parser <* eof) >=> mapM (postparse spec)
    where parser = parseGenericSExpr
                     (sesPAtom spec)
                     (readerMap spec)
                     (buildSkip (comment spec))

{-
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
encodeOne :: SExprParser atom carrier -> carrier -> Text
encodeOne spec c = encodeSExpr (preserial spec c) (sesSAtom spec)

encode :: SExprParser atom carrier -> [carrier] -> Text
encode spec cs = T.concat (map (encodeOne spec) cs)
-}
