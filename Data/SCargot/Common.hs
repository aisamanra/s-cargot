module Data.SCargot.Common ( -- $intro
                           -- * Identifier Syntaxes
                             parseR5RSIdent
                           , parseR6RSIdent
                           , parseR7RSIdent
                           , parseXIDIdentStrict
                           , parseXIDIdentGeneral
                           , parseHaskellIdent
                           , parseHaskellVariable
                           , parseHaskellConstructor
                             -- * Numeric Literal Parsers
                           , signed
                           , prefixedNumber
                           , signedPrefixedNumber
                           , binNumber
                           , signedBinNumber
                           , octNumber
                           , signedOctNumber
                           , decNumber
                           , signedDecNumber
                           , dozNumber
                           , signedDozNumber
                           , hexNumber
                           , signedHexNumber
                             -- ** Numeric Literals for Arbitrary Bases
                           , commonLispNumberAnyBase
                           , gnuM4NumberAnyBase
                           ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative hiding ((<|>), many)
#endif
import           Control.Monad (guard)
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Text (Parser)

-- | Parse an identifier according to the R5RS Scheme standard. This
--   will not normalize case, even though the R5RS standard specifies
--   that all identifiers be normalized to lower case first.
--
--   An R5RS identifier is, broadly speaking, alphabetic or numeric
--   and may include various symbols, but no escapes.
parseR5RSIdent :: Parser Text
parseR5RSIdent =
  T.pack <$> ((:) <$> initial <*> many subsequent <|> peculiar)
  where initial    = letter <|> oneOf "!$%&*/:<=>?^_~"
        subsequent = initial <|> digit <|> oneOf "+-.@"
        peculiar   = string "+" <|> string "-" <|> string "..."

hasCategory :: Char -> [GeneralCategory] -> Bool
hasCategory c cs = generalCategory c `elem` cs

-- | Parse an identifier according to the R6RS Scheme standard. An
--   R6RS identifier may include inline hexadecimal escape sequences
--   so that, for example, @foo@ is equivalent to @f\\x6f;o@, and is
--   more liberal than R5RS as to which Unicode characters it may
--   accept.
parseR6RSIdent :: Parser Text
parseR6RSIdent =
  T.pack <$> ((:) <$> initial <*> many subsequent <|> peculiar)
  where initial = constituent <|> oneOf "!$%&*/:<=>?^_~" <|> inlineHex
        constituent = letter
                   <|> uniClass (\ c -> isLetter c ||
                                        isSymbol c ||
                                        hasCategory c
                                          [ NonSpacingMark
                                          , LetterNumber
                                          , OtherNumber
                                          , DashPunctuation
                                          , ConnectorPunctuation
                                          , OtherPunctuation
                                          , PrivateUse
                                          ])
        inlineHex   = (chr . fromIntegral) <$> (string "\\x" *> hexNumber <* char ';')
        subsequent  = initial <|> digit <|> oneOf "+-.@"
                   <|> uniClass (\ c -> hasCategory c
                                          [ DecimalNumber
                                          , SpacingCombiningMark
                                          , EnclosingMark
                                          ])
        peculiar    = string "+" <|> string "-" <|> string "..." <|>
                      ((++) <$> string "->" <*> many subsequent)
        uniClass :: (Char -> Bool) -> Parser Char
        uniClass sp = satisfy (\ c -> c > '\x7f' && sp c)

-- | Parse an identifier according to the R7RS Scheme standard. An
--   R7RS identifier, in addition to a typical identifier format,
--   can also be a chunk of text surrounded by vertical bars that
--   can contain spaces and other characters. Unlike R6RS, it does
--   not allow escapes to be included in identifiers unless those
--   identifiers are surrounded by vertical bars.
parseR7RSIdent :: Parser Text
parseR7RSIdent =  T.pack <$>
          (  (:) <$> initial <*> many subsequent
         <|> char '|' *> many1 symbolElement <* char '|'
         <|> peculiar
          )
  where initial = letter <|> specInit
        specInit = oneOf "!$%&*/:<=>?^_~"
        subsequent = initial <|> digit <|> specSubsequent
        specSubsequent = expSign <|> oneOf ".@"
        expSign = oneOf "+-"
        symbolElement =  noneOf "\\|"
                     <|> hexEscape
                     <|> mnemEscape
                     <|> ('|' <$ string "\\|")
        hexEscape = chr . fromIntegral <$> (string "\\x" *> hexNumber <* char ';')
        mnemEscape =  '\a' <$ string "\\a"
                  <|> '\b' <$ string "\\b"
                  <|> '\t' <$ string "\\t"
                  <|> '\n' <$ string "\\n"
                  <|> '\r' <$ string "\\r"
        peculiar =  (:[]) <$> expSign
                <|> cons2 <$> expSign <*> signSub <*> many subsequent
                <|> cons3 <$> expSign
                          <*> char '.'
                          <*> dotSub
                          <*> many subsequent
                <|> cons2 <$> char '.' <*> dotSub <*> many subsequent
        dotSub = signSub <|> char '.'
        signSub = initial <|> expSign <|> char '@'
        cons2 a b cs   = a : b : cs
        cons3 a b c ds = a : b : c : ds

-- | Parse a Haskell variable identifier: a sequence of alphanumeric
--   characters, underscores, or single quote that begins with a
--   lower-case letter.
parseHaskellVariable :: Parser Text
parseHaskellVariable =
  T.pack <$> ((:) <$> small <*> many (small <|>
                                      large <|>
                                      digit' <|>
                                      char '\'' <|>
                                      char '_'))
  where small = satisfy isLower
        large = satisfy isUpper
        digit' = satisfy isDigit

-- | Parse a Haskell constructor: a sequence of alphanumeric
--   characters, underscores, or single quote that begins with an
--   upper-case letter.
parseHaskellConstructor :: Parser Text
parseHaskellConstructor =
  T.pack <$> ((:) <$> large <*> many (small <|>
                                      large <|>
                                      digit' <|>
                                      char '\'' <|>
                                      char '_'))
  where small = satisfy isLower
        large = satisfy isUpper
        digit' = satisfy isDigit

-- | Parse a Haskell identifer: a sequence of alphanumeric
--   characters, underscores, or a single quote. This matches both
--   variable and constructor names.
parseHaskellIdent :: Parser Text
parseHaskellIdent =
  T.pack <$> ((:) <$> (large <|> small)
                  <*> many (small <|>
                            large <|>
                            digit' <|>
                            char '\'' <|>
                            char '_'))
  where small = satisfy isLower
        large = satisfy isUpper
        digit' = satisfy isDigit

-- Ensure that a given character has the given Unicode category
hasCat :: [GeneralCategory] -> Parser Char
hasCat cats = satisfy (flip hasCategory cats)

xidStart :: [GeneralCategory]
xidStart = [ UppercaseLetter
           , LowercaseLetter
           , TitlecaseLetter
           , ModifierLetter
           , OtherLetter
           , LetterNumber
           ]

xidContinue :: [GeneralCategory]
xidContinue = xidStart ++ [ NonSpacingMark
                          , SpacingCombiningMark
                          , DecimalNumber
                          , ConnectorPunctuation
                          ]

-- | Parse an identifier of unicode characters of the form
--   @<XID_Start> <XID_Continue>*@, which corresponds strongly
--   to the identifiers found in most C-like languages. Note that
--   the @XID_Start@ category does not include the underscore,
--   so @__foo@ is not a valid XID identifier. To parse
--   identifiers that may include leading underscores, use
--   'parseXIDIdentGeneral'.
parseXIDIdentStrict :: Parser Text
parseXIDIdentStrict = T.pack <$> ((:) <$> hasCat xidStart
                                  <*> many (hasCat xidContinue))

-- | Parse an identifier of unicode characters of the form
--   @(<XID_Start> | '_') <XID_Continue>*@, which corresponds
--   strongly to the identifiers found in most C-like languages.
--   Unlike 'parseXIDIdentStrict', this will also accept an
--   underscore as leading character, which corresponds more
--   closely to programming languages like C and Java, but
--   deviates somewhat from the
--   <http://unicode.org/reports/tr31/ Unicode Identifier and
--   Pattern Syntax standard>.
parseXIDIdentGeneral :: Parser Text
parseXIDIdentGeneral = T.pack <$> ((:) <$> (hasCat xidStart <|> char '_')
                                       <*> many (hasCat xidContinue))

-- | A helper function for defining parsers for arbitrary-base integers.
--   The first argument will be the base, and the second will be the
--   parser for the individual digits.
number :: Integer -> Parser Char -> Parser Integer
number base digits = foldl go 0 <$> many1 digits
  where go x d = base * x + toInteger (value d)
        value c
          | c >= 'a' && c <= 'z' = 0xa + (fromEnum c - fromEnum 'a')
          | c >= 'A' && c <= 'Z' = 0xa + (fromEnum c - fromEnum 'A')
          | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
          | c == '\x218a' = 0xa
          | c == '\x218b' = 0xb
          | otherwise = error ("Unknown letter in number: " ++ show c)

digitsFor :: Int -> [Char]
digitsFor n
  | n <= 10   = take n ['0'..'9']
  | n <= 36   = take (n-10) ['A'..'Z'] ++ take (n-10) ['a'..'z'] ++ ['0'..'9']
  | otherwise = error ("Invalid base for parser: " ++ show n)

anyBase :: Integer -> Parser Integer
anyBase n = number n (oneOf (digitsFor (fromIntegral n)))

-- | A parser for Common Lisp's arbitrary-base number syntax, of
--   the form @#[base]r[number]@, where the base is given in
--   decimal. Note that this syntax begins with a @#@, which
--   means it might conflict with defined reader macros.
commonLispNumberAnyBase :: Parser Integer
commonLispNumberAnyBase = do
  _ <- char '#'
  n <- decNumber
  guard (n >= 2 && n <= 36)
  _ <- char 'r'
  signed (anyBase n)

-- | A parser for GNU m4's arbitrary-base number syntax, of
--   the form @0r[base]:[number]@, where the base is given in
--   decimal.
gnuM4NumberAnyBase :: Parser Integer
gnuM4NumberAnyBase = do
  _ <- string "0r"
  n <- decNumber
  guard (n >= 2 && n <= 36)
  _ <- char ':'
  signed (anyBase n)

sign :: Num a => Parser (a -> a)
sign =  (pure id     <* char '+')
    <|> (pure negate <* char '-')
    <|> pure id

-- | Given a parser for some kind of numeric literal, this will attempt to
--   parse a leading @+@ or a leading @-@ followed by the numeric literal,
--   and if a @-@ is found, negate that literal.
signed :: Num a => Parser a -> Parser a
signed p = ($) <$> sign <*> p

-- | Parses a number in the same way as 'prefixedNumber', with an optional
--   leading @+@ or @-@.
signedPrefixedNumber :: Parser Integer
signedPrefixedNumber = signed prefixedNumber

-- | Parses a number, determining which numeric base to use by examining
--   the literal's prefix: @0x@ for a hexadecimal number, @0z@ for a
--   dozenal number, @0o@ for an octal number, and @0b@ for a binary
--   number (as well as the upper-case versions of the same.) If the
--   base is omitted entirely, then it is treated as a decimal number.
prefixedNumber :: Parser Integer
prefixedNumber =  (string "0x" <|> string "0X") *> hexNumber
              <|> (string "0o" <|> string "0O") *> octNumber
              <|> (string "0z" <|> string "0Z") *> dozNumber
              <|> (string "0b" <|> string "0B") *> binNumber
              <|> decNumber

-- | A parser for non-signed binary numbers
binNumber :: Parser Integer
binNumber = number 2 (char '0' <|> char '1')

-- | A parser for signed binary numbers, with an optional leading @+@ or @-@.
signedBinNumber :: Parser Integer
signedBinNumber = signed binNumber

-- | A parser for non-signed octal numbers
octNumber :: Parser Integer
octNumber = number 8 (oneOf "01234567")

-- | A parser for signed octal numbers, with an optional leading @+@ or @-@.
signedOctNumber :: Parser Integer
signedOctNumber = ($) <$> sign <*> octNumber

-- | A parser for non-signed decimal numbers
decNumber :: Parser Integer
decNumber = number 10 digit

-- | A parser for signed decimal numbers, with an optional leading @+@ or @-@.
signedDecNumber :: Parser Integer
signedDecNumber = ($) <$> sign <*> decNumber

dozDigit :: Parser Char
dozDigit = digit <|> oneOf "AaBb\x218a\x218b"

-- | A parser for non-signed duodecimal (dozenal) numbers. This understands both
--   the ASCII characters @'a'@ and @'b'@ and the Unicode characters @'\x218a'@ (↊)
--   and @'\x218b'@ (↋) as digits with the decimal values @10@ and @11@
--   respectively.
dozNumber :: Parser Integer
dozNumber = number 12 dozDigit

-- | A parser for signed duodecimal (dozenal) numbers, with an optional leading @+@ or @-@.
signedDozNumber :: Parser Integer
signedDozNumber = ($) <$> sign <*> dozNumber

-- | A parser for non-signed hexadecimal numbers
hexNumber :: Parser Integer
hexNumber = number 16 hexDigit

-- | A parser for signed hexadecimal numbers, with an optional leading @+@ or @-@.
signedHexNumber :: Parser Integer
signedHexNumber = ($) <$> sign <*> hexNumber

{- $intro

This module contains a selection of parsers for different kinds of
identifiers and literals, from which more elaborate parsers can be
assembled. These can afford the user a quick way of building parsers
for different atom types.

-}
