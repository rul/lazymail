-- A simple Haskell RFC1342 decoder
-- Copyright (C) 2013 Raúl Benencia <rul@kalgan.cc>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- This module is an ugly hack. It has been poorly tested and it'll
-- probably blow up in your face. You've been warned.
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