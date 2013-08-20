-- This module is part of Lazymail, a Haskell email client.
--
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

module Print where

import Network.Email.Mailbox(Flag(..), Flags)
import Text.ParserCombinators.Parsec.Rfc2822(NameAddr(..))
import Data.Char ( isSpace )

import Email
import Rfc1342

nameLen = 20
ppNameAddr nas = concat $ map ppNameAddr' nas
  where ppNameAddr' na = case nameAddr_name na of
                           Nothing -> nameAddr_addr na
                           Just n  -> decodeField n
                           
ppIndexNameAddr = normalizeLen nameLen . ppNameAddr                           
                           
subjectLen = 90
ppSubject = decodeField
ppIndexSubject = normalizeLen subjectLen . ppSubject
                           
ppFlags :: Flags -> String
ppFlags = map ppFlag

ppFlag :: Flag -> Char
ppFlag SEEN      = 'S'
ppFlag ANSWERED  = 'A'
ppFlag FLAGGED   = 'F'
ppFlag DRAFT     = 'D'
ppFlag FORWARDED = 'P'
ppFlag DELETED   = 'T'
ppFlag (OTHERFLAG [c]) = c

ppSep = "\t"

normalizeLen len cs = if (length cs > len)
                      then shorten len cs
                      else if (length cs < len)
                           then fillWithSpace len cs
                           else cs
                                
fillWithSpace len cs = cs ++ (take (len - length cs) . repeat $ ' ')

-- The following functions are from DynamicLog xmonad-contrib source

-- | Wrap a string in delimiters, unless it is empty.
wrap :: String  -- ^ left delimiter
     -> String  -- ^ right delimiter
     -> String  -- ^ output string
     -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

-- | Pad a string with a leading and trailing space.
pad :: String -> String
pad = wrap " " " "

-- | Trim leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
          
-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = take (n - length end) xs ++ end
  where
    end = "..."
