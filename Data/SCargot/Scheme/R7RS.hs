{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SCargot.Scheme.R7RS where

import           Data.Char (chr, isAlphaNum)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.String (IsString(..))
import           Data.SCargot.Common
import           Data.SCargot.General
import           Data.SCargot.Repr.Basic
import           Data.Word (Word8)
import           Text.Parsec
import           Text.Parsec.Text (Parser)

instance IsString (SchemeAtom c) where
  fromString = ScmIdent . fromString

-- | A Scheme value type. This is strictly larger than just
--   'atoms', as they may include things like vectors or
--   labeled data, which must be able to refer to yet other
--   s-expressions. Thus, the SchemeAtom type must be able
--   to itself refer to the carrier type in which it is
--   contained.
data SchemeAtom carrier
  = ScmIdent Text
  | ScmBool Bool
  | ScmString Text
  | ScmNum Integer
  | ScmChar Char
  | ScmVec [carrier (SchemeAtom carrier)]
  | ScmByteVec [Word8]
  | ScmLabeledDatum Int (carrier (SchemeAtom carrier))
  | ScmLabelReference Int

-- | Scheme has a lot of numbers.
data SchemeNumber
  = ScmNumber
  | ScmComplexNumber  Double Double
  | ScmRealNumber     Double
  | ScmRationalNumber Rational
  | ScmInteger        Integer
    deriving (Eq, Show)

deriving instance Show (c (SchemeAtom c)) => Show (SchemeAtom c)
deriving instance Eq (c (SchemeAtom c)) => Eq (SchemeAtom c)

badSpec :: SExprSpec (SchemeAtom SExpr) (SExpr (SchemeAtom SExpr))
badSpec = mkSpec (ScmIdent . T.pack <$> many1 (satisfy isAlphaNum)) undefined

withQuasiQuote :: SExprSpec (SchemeAtom SExpr) (SExpr (SchemeAtom SExpr))
               -> SExprSpec (SchemeAtom SExpr) (SExpr (SchemeAtom SExpr))
withQuasiQuote spec = addReader '`' (fmap (go "quasiquote"))
                    $ addReader ',' unquote
                    $ addReader '\'' (fmap (go "quote"))
                    $ spec
   where go name s = name ::: s ::: Nil
         unquote p = char '@' *> fmap (go "unquote-splicing") p
                  <|> fmap (go "unquote") p

octoReader :: Reader (SchemeAtom SExpr)
octoReader pSexpr =
      string "true"  *> pure (A (ScmBool True))
  <|> string "false" *> pure (A (ScmBool False))
  <|> char 't' *> pure (A (ScmBool True))
  <|> char 'f' *> pure (A (ScmBool False))
  <|> char '\\' *> fmap (A . ScmChar) characterConstant
  <|> char '(' *> fmap (A . ScmVec) (vector pSexpr)
  <|> string "u8(" *> fmap A bytevec
  <|> do n <- read <$> many1 digit
         (char '#' *> pure (A (ScmLabelReference n)) <|>
          char '=' *> fmap (A . ScmLabeledDatum n) pSexpr)

vector :: Parser (SExpr (SchemeAtom SExpr)) -> Parser [SExpr (SchemeAtom SExpr)]
vector pSExpr =
  (char ')' *> pure []) <|> ((:) <$> pSExpr <*> vector pSExpr)

bytevec :: Parser (SchemeAtom SExpr)
bytevec = undefined

characterConstant :: Parser Char
characterConstant = namedCharacter
                 <|> (chr . fromInteger <$> (char 'x' *> hexNumber))
                 <|> anyCharacter
  where namedCharacter =  string "alarm"     *> pure '\x07'
                      <|> string "backspace" *> pure '\x08'
                      <|> string "delete"    *> pure '\x7f'
                      <|> string "escape"    *> pure '\x1b'
                      <|> string "newline"   *> pure '\x0a'
                      <|> string "null"      *> pure '\x00'
                      <|> string "return"    *> pure '\x0d'
                      <|> string "space"     *> pure ' '
                      <|> string "tab"       *> pure '\x09'
        anyCharacter = anyToken

r7rsNum :: Int -> Parser Int
r7rsNum radix = prefix <*> complex
  where prefix = radix <*> exactness <|> exactness <*> radix
        complex =  real
               <|> real <* char '@' <*> real
               <|> real <* char '+' <*> ureal <* char 'i'
               <|> real <* char '-' <*> ureal <* char 'i'
               <|> real <* char '+' <* char 'i'
               <|> real <* char '-' <* char 'i'
               <|> real <*> infnan <* char 'i'
               <|> char '+' *> ureal <* char 'i'
               <|> char '-' *> ureal <* char 'i'
               <|> infnan <* char 'i'
               <|> string "+i"
               <|> string "-i"
        real = ($) <$> sign <*> ureal
            <|> infnan
