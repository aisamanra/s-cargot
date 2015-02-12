module Data.SExpression.Rivest where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text (Text)
import qualified Data.Text as T

newtype Atom = Atom { fromAtom :: ByteString } deriving (Eq, Show, Read)

pToken :: Parser ByteString
pToken = undefined

pQuoted :: Maybe Int -> Parser ByteString
pQuoted = do
  char '"'
  ss <- many1 quoteChar
  char '"'
  return ss

pHex :: Parser ByteString
pHex = undefined

pVerbatim :: Int -> Parser ByteString
pVerbatim = do
  char ':'
  take n

pBase64Verbatim :: Parser ByteString
pBase64 :: Parser ByteString
