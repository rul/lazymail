{- Printing utilities.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Lazymail.Print where

import Data.Char (isSpace)
import Data.List (intercalate)

import Lazymail.Email
import Codec.Text.Rfc1342
import Lazymail.Types(Flag(..), Flags, ComposeFields(..), ComposeState(..))

unquote xs= if (head xs == '"' && last xs == '"') then (tail . init) xs else xs

ppField = flat . decodeField

{- Pretty print a RFC822 date format -}


fromLen :: Int
fromLen = 20

maxFlags :: Int
maxFlags = 4

flat xs = intercalate " " $ map (dropWhile isSpace) $ map (filter (/= '\r')) $ lines xs

ppFlags :: Flags -> String
ppFlags = map ppFlag

ppFlag :: Flag -> Char
ppFlag NEW       = 'N'
ppFlag SEEN      = 'S'
ppFlag ANSWERED  = 'A'
ppFlag FLAGGED   = 'F'
ppFlag DRAFT     = 'D'
ppFlag FORWARDED = 'P'
ppFlag DELETED   = 'T'
ppFlag (OTHERFLAG [c]) = c

ppComposeState cs = ppComposeFields False (composeFields cs) ++
                    [("Body file name: " ++) $ maybe "-" id $ bodyFileName cs]

ppComposeFields removeEmpty cf | removeEmpty == False = l
                               | otherwise = filter (\str -> (last str) /= '-') l
  where l = [ ("From: " ++) $ maybe "-" id $ fromField cf
            , ("To: " ++) $ maybe "-" id $ toField cf
            , ("Cc: " ++) $ maybe "-" id $ ccField cf
            , ("Bcc: " ++) $ maybe "-" id $ bccField cf
            , ("Reply-To: " ++) $ maybe "-" id $ replyToField cf
            , ("Subject: " ++) $ maybe "-" id $ subjectField cf
            ]

ppSep = "  "

normalizeLen len cs | (length cs > len) = shorten len cs
                    | otherwise = if (length cs < len)
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
