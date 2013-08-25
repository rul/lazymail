{- A simple RFC1342 decoder.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}
module Rfc1342 (decodeField) where

import qualified Codec.Binary.Base64 as B64
import qualified Codec.Binary.QuotedPrintable as QP

import Data.Char (toLower, isSpace, chr)
import Data.List(isPrefixOf)
import Data.Word (Word8)

import Data.Encoding(decodeString)

-- Encoding imports. If you want to support more encodings, just add'em here.
import Data.Encoding.UTF8
import Data.Encoding.ISO88591
import Data.Encoding.ISO88592
import Data.Encoding.ISO88598
import Data.Encoding.ISO88599

decodeField :: String -> String
decodeField ('=':'?':cs) = decodeWithCharset dec rest
  where (encoding, rest) = span (\c -> c /= '?') cs
        dec = case (map toLower encoding) of
          "utf-8"      -> decodeString UTF8
          "iso-8859-1" -> decodeString ISO88591
          "iso-8859-2" -> decodeString ISO88592
          "iso-8859-8" -> decodeString ISO88598
          "iso-8859-9" -> decodeString ISO88599
          _            -> id
decodeField []           = []
decodeField (c:cs)       = c:decodeField cs

decodeWithCharset dec [] = []
decodeWithCharset dec ('?':c:'?':cs) | toLower c == 'b' = dataDecodeWith B64.decode
                                     | toLower c == 'q' = dataDecodeWith QP.decode
                                     | otherwise        = cs
  where (encoded, rest') = span  (\c -> c /= '?') cs
        rest = if "?=" `isPrefixOf` rest'
               then drop 2 rest'
               else rest'
        dataDecodeWith datadec = (_2spc . dec . unwrap . datadec $ encoded) ++ (decodeField $ dropWhile isSpace rest)

unwrap :: Maybe [Word8] -> String
unwrap Nothing    = []
unwrap (Just str) = bytesToString str

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

-- Sometimes an underscore represents the SPACE character
_2spc = map (\x -> if x == '_' then ' ' else x)