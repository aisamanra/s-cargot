module Data.SCargot.Scheme.R7RS where

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
    deriving (Eq, Show)

withQuasiQuote :: SExprSpec (SchemeAtom SExpr) (SExpr (SchemeAtom Sexpr))
               -> SExprSpec (SchemeAtom SExpr) (SExpr (SchemeAtom SExpr))
withQuasiQuote spec = addReader '`' (fmap (go "quasiquote"))
                    $ addReader ',' unquote
                    $ spec
   where go name s = name ::: s ::: Nil
         unquote p = char '@' *> fmap (go "unquote-splicing")
                  <|> fmap (go "unquote")

octoReader :: Reader (SExpr (SchemeAtom SExpr))
octoReader pSexpr =
      string "true"  *> pure (ScmBool True)
  <|> string "false" *> pure (ScmBool False)
  <|> char 't' *> pure (ScmBool True)
  <|> char 'f' *> pure (ScmBool False)
  <|> char '\\' *> characterConstant
  <|> char '(' *> fmap ScmVec (vector pSexpr)
  <|> string "u8(" *> bytevec
  <|> do n <- read <$> many1 digit
         (char '#' *> ScmLabelReference n <|>
          char '=' *> fmap (ScmLabeledDatum n) pSexpr)

vector :: Parser (SExpr (SchemeAtom SExpr)) -> Parser [SExpr (SchemeAtom SExpr)]
vector pSexpr =
  (char ')' *> pure []) <|> ((:) <$> pSExpr <*> vector pSexpr)
