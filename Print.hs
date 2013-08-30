{- Printing utilities.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Print where

import Data.Char (isSpace)
import Data.List (intercalate)
import Network.Email.Mailbox(Flag(..), Flags)
import Text.ParserCombinators.Parsec.Rfc2822(NameAddr(..))

import Email
import Rfc1342

nameLen = 20
ppNameAddr nas = intercalate ", " $ map ppNameAddr' nas
  where ppNameAddr' na = case nameAddr_name na of
                           Nothing -> nameAddr_addr na
                           Just n  -> (decodeField n) ++ " <" ++ nameAddr_addr na ++ ">"

ppIndexNameAddr nas = normalizeLen nameLen $ concat $ map ppNameAddr' nas
  where ppNameAddr' na = case nameAddr_name na of
                           Nothing -> nameAddr_addr na
                           Just n  -> (decodeField n)

subjectLen = 90
ppSubject = flat . decodeField

flat xs = intercalate " " $ map (dropWhile isSpace) $ map (filter (/= '\r')) $ lines xs

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
