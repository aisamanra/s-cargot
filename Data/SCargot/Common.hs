module Data.SCargot.Common ( number
                           , decNumber
                           , hexNumber
                           , octNumber
                           , sign
                           -- * Lisp Identifier Syntaxes
                           , parseR5RSIdent
                           , parseR6RSIdent
                           , parseR7RSIdent
                           ) where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Char (satisfy)
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
--   so that, for example, 'foo' is equivalent to 'f\x6f;o', and is
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
--   not allow escapes to be included in identifiers that are not
--   surrounded by vertical bars.
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

-- | A helper function for defining parsers for arbitrary-base integers.
--   The first argument will be the base, and the second will be the
--   parser for the individual digits.
number :: Integer -> Parser Char -> Parser Integer
number base digits = foldl go 0 <$> many1 digits
  where go x d = base * x + toInteger (value d)
        value c
          | c == 'a' || c == 'A' = 0xa
          | c == 'b' || c == 'B' = 0xb
          | c == 'c' || c == 'C' = 0xc
          | c == 'd' || c == 'D' = 0xd
          | c == 'e' || c == 'E' = 0xe
          | c == 'f' || c == 'F' = 0xf
          | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
          | otherwise = error ("Unknown letter in number: " ++ show c)

-- | A parser for bare binary numbers
binNumber :: Parser Integer
binNumber = number 2 (char '0' <|> char '1')

-- | A parser for bare octal numbers
octNumber :: Parser Integer
octNumber = number 8 digit

-- | A parser for bare decimal numbers
decNumber :: Parser Integer
decNumber = number 10 digit

-- | A parser for bare hexadecimal numbers
hexNumber :: Parser Integer
hexNumber = number 16 hexDigit

-- | A parser for numeric signs, represented as a function from numbers
--   to numbers. It will parse '+' as the identity function, '-', as
--   'negate', or consume no input and return the identity function.
--   This can be combined with other numeric literals to implement
--   signedness:
--
--   > myNum = go <$> sign <*> decNumber
--   >   where go s n = s n
sign :: Num a => Parser (a -> a)
sign =  (pure id     <* char '+')
    <|> (pure negate <* char '-')
    <|> pure id
