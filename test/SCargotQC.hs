{-# LANGUAGE OverloadedStrings #-}

module Data.SCargot.ReprQC (reprQC) where

import Data.SCargot ( SExprParser
                    , SExprPrinter
                    , mkParser
                    , flatPrint
                    , encodeOne
                    , decodeOne
                    , asRich
                    , asWellFormed
                    )
import Data.SCargot.Repr ( SExpr(..)
                         , RichSExpr
                         , fromRich
                         , toRich
                         , WellFormedSExpr(..)
                         , fromWellFormed
                         , toWellFormed
                         )
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Text.Parsec (char)
import Text.Parsec.Text (Parser)

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

parser :: SExprParser () (SExpr ())
parser = mkParser (() <$ char 'X')

printer :: SExprPrinter () (SExpr ())
printer = flatPrint (const "X")

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

encDecRich :: RichSExpr () -> Bool
encDecRich s = decodeOne (asRich parser) (encodeOne printer (fromRich s))
                == Right s

encDecWF :: WellFormedSExpr () -> Bool
encDecWF s = decodeOne (asWellFormed parser) (encodeOne printer (fromWellFormed s))
               == Right s

reprQC :: IO ()
reprQC = do
  putStrLn "The SExpr <--> Rich translation should be isomorphic"
  quickCheck richIso
  quickCheck richIsoBk
  putStrLn "The SExpr <--> WF translation should be near-isomorphic"
  quickCheck wfIso
  quickCheck wfIsoBk
  putStrLn "This should be true when parsing, as well"
  quickCheck encDec
  quickCheck encDecRich
  quickCheck encDecWF
