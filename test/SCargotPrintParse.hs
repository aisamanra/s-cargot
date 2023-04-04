{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Either
import           Data.SCargot
import           Data.SCargot.Comments
import           Data.SCargot.Repr
import           Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Exit
import           Test.HUnit
import           Text.Parsec as P
import           Text.Parsec.Text (Parser)
import           Text.Printf ( printf )


main = do
  putStrLn "Parsing a large S-expression"
  srcs <- mapM (\n -> (,) n <$> TIO.readFile n) [ "test/small-sample.sexp"
                                                , "test/med-sample.sexp"
                                                , "test/med2-sample.sexp"
                                                , "test/big-sample.sexp"
                                                ]
  counts <- runTestTT $ TestList $
            let
              -- l2p = list of 2 pairs
              l2p = (SCons
                      (SCons
                        (SAtom (AIdent "hi"))
                        (SAtom (AIdent "hallo")))
                      (SCons
                        (SAtom (AIdent "world"))
                        (SAtom (AIdent "welt"))))
              -- l3ep = list of 3 ending in a pair
              l3ep = (SCons
                       (SAtom (AIdent "hi"))
                       (SCons
                         (SAtom (AIdent "world"))
                         (SCons
                           (SAtom (AIdent "hallo"))
                           (SAtom (AIdent "welt")))))
              -- l3 = list of 3
              l3 = (SCons
                     (SAtom (AIdent "hi"))
                     (SCons
                       (SAtom (AIdent "world"))
                       (SCons
                         (SAtom (AIdent "hallo"))
                         SNil)))
              -- l2 = list of 2
              l2 = (SCons
                     (SAtom (AIdent "hi"))
                     (SCons
                       (SAtom (AIdent "world"))
                       SNil))
              -- l1 = list of 1
              l1 = (SCons (SAtom (AIdent "hi")) SNil)
              pair = (SCons (SAtom (AIdent "hi")) (SAtom (AIdent "world")))
            in
            [ TestLabel "basic checks" $ TestList
              [ TestLabel "flat print" $ TestList
                [ TestLabel "flatprint SNil" $ "()" ~=? printSExpr SNil
                , TestLabel "flatprint SAtom" $ "hi" ~=? printSExpr (SAtom (AIdent "hi"))
                , TestLabel "flatprint pair" $ "(hi . world)" ~=?
                  printSExpr pair
                , TestLabel "flatprint list of 1" $ "(hi)" ~=?
                  printSExpr l1
                , TestLabel "flatprint list of 2" $ "(hi world)" ~=?
                  printSExpr l2
                , TestLabel "flatprint list of 2 pairs" $ "((hi . hallo) world . welt)" ~=?
                  printSExpr l2p
                , TestLabel "flatprint list of 3 ending in a pair" $ "(hi world hallo . welt)" ~=?
                  printSExpr l3ep
                , TestLabel "flatprint list of 3" $ "(hi world hallo)" ~=?
                  printSExpr l3
                ]

              , TestLabel "pretty print width 40" $
                let pprintIt = pprintSExpr 40 Swing in TestList
                [ TestLabel "pretty print SNil" $ "()" ~=? pprintIt SNil
                , TestLabel "pretty print SAtom" $ "hi" ~=? pprintIt (SAtom (AIdent "hi"))
                , TestLabel "pretty print pair" $ "(hi . world)" ~=?
                  pprintIt pair
                , TestLabel "pretty print list of 1" $ "(hi)" ~=?
                  pprintIt l1
                , TestLabel "pretty print list of 2" $ "(hi world)" ~=?
                  pprintIt l2
                , TestLabel "pretty print list of 2 pairs" $
                  "((hi . hallo) world . welt)" ~=?
                  pprintIt l2p
                , TestLabel "pretty print list of 3 ending in a pair" $
                  "(hi world hallo . welt)" ~=?
                  pprintIt l3ep
                , TestLabel "pretty print list of 3" $ "(hi world hallo)" ~=?
                  pprintIt l3
                ]

              , TestLabel "pretty print width 10" $
                let pprintIt = pprintSExpr 10 Swing in TestList
                [ TestLabel "pretty print SNil" $ "()" ~=? pprintIt SNil
                , TestLabel "pretty print SAtom" $ "hi" ~=? pprintIt (SAtom (AIdent "hi"))
                , TestLabel "pretty print pair" $ "(hi . world)" ~=?
                  pprintIt pair
                , TestLabel "pretty print list of 1" $ "(hi)" ~=?
                  pprintIt l1
                , TestLabel "pretty print list of 2" $ "(hi world)" ~=?
                  pprintIt l2
                , TestLabel "pretty print list of 2 pairs" $
                  "((hi . hallo) \n world . welt)" ~=?
                  pprintIt l2p
                , TestLabel "pretty print list of 3 ending in a pair" $
                  "(hi \n world\n hallo . welt)" ~=?
                  pprintIt l3ep
                , TestLabel "pretty print list of 3" $ "(hi \n world\n hallo)" ~=?
                  pprintIt l3
                ]

              , TestLabel "unconstrained print" $
                let pprintIt = ucPrintSExpr Swing in TestList
                [ TestLabel "pretty print SNil" $ "()" ~=? pprintIt SNil
                , TestLabel "pretty print SAtom" $ "hi" ~=? pprintIt (SAtom (AIdent "hi"))
                , TestLabel "pretty print pair" $ "(hi . world)" ~=?
                  pprintIt pair
                , TestLabel "pretty print list of 1" $ "(hi)" ~=?
                  pprintIt l1
                , TestLabel "pretty print list of 2" $ "(hi world)" ~=?
                  pprintIt l2
                , TestLabel "pretty print list of 2 pairs" $
                  "((hi . hallo)\n world\n . welt)" ~=?
                  pprintIt l2p
                , TestLabel "pretty print list of 3 ending in a pair" $
                  "(hi world hallo . welt)" ~=?
                  pprintIt l3ep
                , TestLabel "pretty print list of 3" $ "(hi world hallo)" ~=?
                  pprintIt l3
                ]

              ]
            , TestLabel "round-trip" $ TestList $
              concatMap (\t -> map t srcs) $
              [ testParsePrint
              ]
            ]
  if errors counts + failures counts > 0
  then exitFailure
  else exitSuccess


testParsePrint :: (String, T.Text) -> Test
testParsePrint (n,s) = TestList
                       [ testParseFlatPrint n s

                       , testParseUnconstrainedPrint Swing n s
                       , testParseUnconstrainedPrint Align n s

                       , testParsePPrint 80 Swing n s
                       , testParsePPrint 60 Swing n s
                       , testParsePPrint 40 Swing n s
                       , testParsePPrint 20 Swing n s
                       , testParsePPrint 15 Swing n s
                       , testParsePPrint 10 Swing n s

                       , testParsePPrint 80 Align n s
                       , testParsePPrint 40 Align n s
                       , testParsePPrint 10 Align n s
                       ]


testParseFlatPrint testName src =
    testRoundTrip (testName <> " flat print")
                      (fromRight (error "Failed parse") . parseSExpr)
                      printSExpr
                      stripAllText
                      src

testParseUnconstrainedPrint indentStyle testName src =
    testRoundTrip (testName <> " unconstrained print")
                      (fromRight (error "Failed parse") . parseSExpr)
                      (ucPrintSExpr indentStyle)
                      stripAllText
                      src

testParsePPrint width indentStyle testName src =
    testRoundTrip (testName <> " pretty print")
                      (fromRight (error "Failed parse") . parseSExpr)
                      (pprintSExpr width indentStyle)
                      stripAllText
                      src

stripAllText = T.unwords . concatMap T.words . T.lines

testRoundTrip nm there back prep src = TestList
  [ TestLabel (nm <> " round trip") $
    TestCase $ (prep src) @=? (prep $ back $ there src)

  , TestLabel (nm <> " round trip twice") $
    TestCase $ (prep src) @=? (prep $ back $ there $ back $ there src)
  ]


------------------------------------------------------------------------

data FAtom = AIdent String
           | AQuoted String
           | AString String
           | AInt Integer
           | ABV Int Integer
           deriving (Eq, Show)


string :: String -> SExpr FAtom
string = SAtom . AString

-- | Lift an unquoted identifier.
ident :: String -> SExpr FAtom
ident = SAtom . AIdent

-- | Lift a quoted identifier.
quoted :: String -> SExpr FAtom
quoted = SAtom . AQuoted

-- | Lift an integer.
int :: Integer -> SExpr FAtom
int = SAtom . AInt


printAtom :: FAtom -> T.Text
printAtom a =
  case a of
    AIdent s -> T.pack s
    AQuoted s -> T.pack ('\'' : s)
    AString s -> T.pack (show s)
    AInt i -> T.pack (show i)
    ABV w val -> formatBV w val


printSExpr :: SExpr FAtom -> T.Text
printSExpr = encodeOne (flatPrint printAtom)

pprintSExpr :: Int -> Indent -> SExpr FAtom -> T.Text
pprintSExpr w i = encodeOne (setIndentStrategy (const i) $
                             setMaxWidth w $
                             setIndentAmount 1 $
                             basicPrint printAtom)

ucPrintSExpr :: Indent -> SExpr FAtom -> T.Text
ucPrintSExpr i = encodeOne (setIndentStrategy (const i) $
                            setIndentAmount 1 $
                            unconstrainedPrint printAtom)

getIdent :: FAtom -> Maybe String
getIdent (AIdent s) = Just s
getIdent _ = Nothing

formatBV :: Int -> Integer -> T.Text
formatBV w val = T.pack (prefix ++ printf fmt val)
  where
    (prefix, fmt)
      | w `rem` 4 == 0 = ("#x", "%0" ++ show (w `div` 4) ++ "x")
      | otherwise = ("#b", "%0" ++ show w ++ "b")

parseIdent :: Parser String
parseIdent = (:) <$> first <*> P.many rest
  where first = P.letter P.<|> P.oneOf "+-=<>_"
        rest = P.letter P.<|> P.digit P.<|> P.oneOf "+-=<>_"

parseString :: Parser String
parseString = do
  _ <- P.char '"'
  s <- P.many (P.noneOf ['"'])
  _ <- P.char '"'
  return s

parseBV :: Parser (Int, Integer)
parseBV = P.char '#' >> ((P.char 'b' >> parseBin) P.<|> (P.char 'x' >> parseHex))
  where parseBin = P.oneOf "10" >>= \d -> parseBin' (1, if d == '1' then 1 else 0)

        parseBin' :: (Int, Integer) -> Parser (Int, Integer)
        parseBin' (bits, x) = do
          P.optionMaybe (P.oneOf "10") >>= \case
            Just d -> parseBin' (bits + 1, x * 2 + (if d == '1' then 1 else 0))
            Nothing -> return (bits, x)

        parseHex = (\s -> (length s * 4, read ("0x" ++ s))) <$> P.many1 P.hexDigit

parseAtom :: Parser FAtom
parseAtom
  =   AIdent      <$> parseIdent
  P.<|> AQuoted     <$> (P.char '\'' >> parseIdent)
  P.<|> AString     <$> parseString
  P.<|> AInt . read <$> P.many1 P.digit
  P.<|> uncurry ABV <$> parseBV

parserLL :: SExprParser FAtom (SExpr FAtom)
parserLL = withLispComments (mkParser parseAtom)

parseSExpr :: T.Text -> Either String (SExpr FAtom)
parseSExpr = decodeOne parserLL
