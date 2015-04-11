module Data.SCargot.Rivest where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text (Text)
import qualified Data.Text as T

pToken :: Parser ByteString
pToken = do
  x <- char (isAlpha || isTokenPunct)
  xs <- takeWhile1 (isAlpha || isDigit || isTokenPunct)

isTokenPunct :: Char -> Bool
isTokenPunct c = c `elem` "-./_:*+="

pWithLength :: Parser ByteString
pWithLength = do
  n <- takeWhile1 isDigit
  pFindType (Just (read (T.unpack n)))

pFindType :: Maybe Int -> Parser ByteString
pFindType len = do
  c <- peekChar
  case c of
    ':' -> case len of
             Just l  -> pVerbatim l
             Nothing -> fail "Verbatim encoding without length given"
    '"' -> pQuoted len
    '#' -> pHex len
    '{' -> pBase64Verbatim len
    '|' -> pBase64 len
    _   -> case len of
             Just _  -> fail "Unexpected length field"
             Nothing -> pToken

pQuoted :: Maybe Int -> Parser ByteString
pQuoted = do
  char '"'
  ss <- many1 quoteChar
  char '"'
  return ss

pHex :: Parser ByteString
pHex = do


pVerbatim :: Int -> Parser ByteString
pVerbatim = do
  char ':'
  take n

pBase64Verbatim :: Maybe Int -> Parser ByteString
pBase64Verbatim = undefined

pBase64 :: Maybe Int -> Parser ByteString
pBase64 = undefined
