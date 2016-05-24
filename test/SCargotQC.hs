{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SCargot
import Data.SCargot.Comments
import Data.SCargot.Repr

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Text.Parsec (char)
import           Text.Parsec.Text (Parser)

instance Arbitrary a => Arbitrary (SExpr a) where
  arbitrary = sized $ \n ->
    if n <= 0
       then pure SNil
       else oneof [ SAtom <$> arbitrary
                  , do
                      k <- choose (0, n)
                      elems <- sequence [ resize (n-k) arbitrary
                                        | _ <- [0..k]
                                        ]
                      tail <- oneof [ SAtom <$> arbitrary
                                    , pure SNil
                                    ]
                      pure (foldr SCons tail elems)
                  ]

instance Arbitrary a => Arbitrary (RichSExpr a) where
  arbitrary = toRich `fmap` arbitrary

instance Arbitrary a => Arbitrary (WellFormedSExpr a) where
  arbitrary = sized $ \n ->
    oneof [ WFSAtom <$> arbitrary
          , do
              k <- choose (0, n)
              WFSList <$> sequence
                [ resize (n-k) arbitrary
                | _ <- [0..k]
                ]
          ]

data EncodedSExpr = EncodedSExpr
  { encoding :: Text
  , original :: SExpr ()
  } deriving (Eq, Show)

instance Arbitrary EncodedSExpr where
  arbitrary = do
    sexpr :: SExpr () <- arbitrary
    let chunks = T.words (encodeOne printer sexpr)
    whitespace <- sequence [ mkWs | _ <- chunks ]
    pure (EncodedSExpr { encoding = T.concat (zipWith (<>) chunks whitespace)
                       , original = sexpr
                       })
    where mkWs = do
            n :: Int <- choose (1, 10)
            T.pack <$> sequence [ elements " \t\r\n"
                                | _ <- [0..n]
                                ]

parser :: SExprParser () (SExpr ())
parser = mkParser (() <$ char 'X')

printer :: SExprPrinter () (SExpr ())
printer = flatPrint (const "X")

prettyPrinter :: SExprPrinter () (SExpr ())
prettyPrinter = basicPrint (const "X")


richIso :: SExpr () -> Bool
richIso s = fromRich (toRich s) == s

richIsoBk :: RichSExpr () -> Bool
richIsoBk s = toRich (fromRich s) == s


wfIso :: SExpr () -> Bool
wfIso s = case toWellFormed s of
  Left _  -> True
  Right y -> s == fromWellFormed y

wfIsoBk :: WellFormedSExpr () -> Bool
wfIsoBk s = toWellFormed (fromWellFormed s) == Right s


encDec :: SExpr () -> Bool
encDec s = decodeOne parser (encodeOne printer s) == Right s

encDecPretty :: SExpr () -> Bool
encDecPretty s = decodeOne parser (encodeOne prettyPrinter s) == Right s

decEnc :: EncodedSExpr -> Bool
decEnc s = decodeOne parser (encoding s) == Right (original s)


encDecRich :: RichSExpr () -> Bool
encDecRich s = decodeOne (asRich parser) (encodeOne printer (fromRich s))
                == Right s

encDecRichPretty :: RichSExpr () -> Bool
encDecRichPretty s = decodeOne (asRich parser)
                               (encodeOne prettyPrinter (fromRich s))
                       == Right s

decEncRich :: EncodedSExpr -> Bool
decEncRich s = decodeOne (asRich parser) (encoding s) == Right (toRich (original s))


encDecWF :: WellFormedSExpr () -> Bool
encDecWF s = decodeOne (asWellFormed parser) (encodeOne printer (fromWellFormed s))
               == Right s

encDecWFPretty :: WellFormedSExpr () -> Bool
encDecWFPretty s =
  decodeOne (asWellFormed parser) (encodeOne prettyPrinter (fromWellFormed s))
    == Right s

decEncWF :: EncodedSExpr -> Bool
decEncWF s = decodeOne (asWellFormed parser) (encoding s) == toWellFormed (original s)


insertComments :: Text -> Text -> Text -> Text
insertComments lc rc sexpr =
  T.replace " " (" " <> lc <> "blahblahblah" <> rc <> " ") sexpr

encDecLineComments :: SExpr () -> Bool
encDecLineComments s =
  decodeOne (withLispComments parser)
            (insertComments ";" "\n" (encodeOne printer s)) == Right s

encDecBlockComments :: SExpr () -> Bool
encDecBlockComments s =
  decodeOne (withHaskellBlockComments parser)
            (insertComments "{-" "-}" (encodeOne printer s)) == Right s

-- Sometimes we generate really huge test cases, which can take a really
-- long time to process---especially when we're modifying the whitespace
-- to produce weird anomalous S-expressions. So, we make the size parameter
-- a bit smaller for good measure.
reallyQuickCheck :: Testable prop => prop -> IO ()
reallyQuickCheck = quickCheckWith stdArgs { maxSize = 25 }

main :: IO ()
main = do
  putStrLn "The SExpr <--> Rich translation should be isomorphic"
  quickCheck richIso
  quickCheck richIsoBk

  putStrLn "The SExpr <--> WF translation should be near-isomorphic"
  quickCheck wfIso
  quickCheck wfIsoBk

  putStrLn "This should be true when parsing, as well"
  quickCheck encDec
  reallyQuickCheck decEnc
  quickCheck encDecRich
  reallyQuickCheck decEncRich
  quickCheck encDecWF
  reallyQuickCheck decEncWF

  putStrLn "And it should be true if pretty-printed"
  reallyQuickCheck encDecPretty
  reallyQuickCheck encDecRichPretty
  reallyQuickCheck encDecWFPretty

  putStrLn "Comments should not affect parsing"
  reallyQuickCheck encDecLineComments
  reallyQuickCheck encDecBlockComments
