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

parseR5RSIdent :: Parser Text
parseR5RSIdent =
  T.pack <$> ((:) <$> initial <*> many subsequent <|> peculiar)
  where initial    = letter <|> oneOf "!$%&*/:<=>?^_~"
        subsequent = initial <|> digit <|> oneOf "+-.@"
        peculiar   = string "+" <|> string "-" <|> string "..."

hasCategory :: Char -> [GeneralCategory] -> Bool
hasCategory c cs = generalCategory c `elem` cs

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
        symbolElement = undefined
        peculiar = undefined

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
