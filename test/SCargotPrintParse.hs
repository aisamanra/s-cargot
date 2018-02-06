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
import           Test.HUnit
import           Text.Parsec as P
import           Text.Parsec.Text (Parser)
import           Text.Printf ( printf )

main = do
  putStrLn "Parsing a large S-expression"
  runTestTT $ TestList [ TestLabel "parse-print flat round-trip" test1
                       , TestLabel "parse-print pretty round-trip 10" $ test2 10
                       , TestLabel "parse-print pretty round-trip 15" $ test2 15
                       , TestLabel "parse-print pretty round-trip 20" $ test2 20
                       , TestLabel "parse-print pretty round-trip 40" $ test2 40
                       , TestLabel "parse-print pretty round-trip 60" $ test2 60
                       , TestLabel "parse-print pretty round-trip 80" $ test2 80
                       ]

test1 = TestCase $ do
  src <- TIO.readFile "sample.sexp"
  let sexpRes = parseSExpr src
  assertBool ("parse errors: " <>
              (either id (const "none") sexpRes)) $ isRight sexpRes
  let parseOut = sexpRes >>= return . printSExpr >>= parseSExpr
  assertBool ("parse out errors: " <>
              (either id (const "none") sexpRes)) $ isRight parseOut
  assertEqual "round-trip" sexpRes parseOut

test2 width = TestCase $ do
  src <- TIO.readFile "sample.sexp"
  let sexpRes = parseSExpr src
  assertBool ("parse errors: " <>
              (either id (const "none") sexpRes)) $ isRight sexpRes

  {-
  putStrLn ""
  putStrLn "----------------------------------------------------------------------"
  putStrLn $ "File size: " <> (show $ T.length src)
  putStrLn $ "Parsed: " <> show sexpRes
  putStrLn ""
  putStrLn $ "Flat Printed: " <> (show $ sexpRes >>= return . printSExpr)
  putStrLn ""
  putStrLn $ "Pretty Printed @ " <> show width <> ": "
  mapM_ TIO.putStrLn  (T.lines $ pprintSExpr width $ fromRight SNil sexpRes)
  putStrLn ""
  -}

  let parseOut = sexpRes >>= return . pprintSExpr width >>= parseSExpr
  assertBool ("parse out errors: " <>
              (either id (const "none") sexpRes)) $ isRight parseOut
  assertEqual "round-trip" sexpRes parseOut


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

pprintSExpr :: Int -> SExpr FAtom -> T.Text
pprintSExpr w = encodeOne (setMaxWidth w $ setIndentAmount 1 $ basicPrint printAtom)

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
