{- Email accessors.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}
module Lazymail.Email where

import Codec.MIME.Type(MIMEValue(..), MIMEContent(..), showMIMEType, Type(..), MIMEType(..))
import Data.Char(toLower)
import Data.List(find)

getBody :: MIMEValue -> String
getBody msg =
  case mime_val_content msg of
    Single c -> c
    Multi mvs -> case firstTextPart mvs of
      Just mv -> unwrapContent . mime_val_content $ mv
      Nothing -> "This email has no displayable content."
  where
    unwrapContent (Single c) = c

-- hackish function for showing the email. In he future the logic of this
-- function should be improved.
firstTextPart []       = Nothing
firstTextPart (mv:mvs) = case mime_val_content mv of
  Single c   -> if isText mv then Just mv else firstTextPart mvs
  Multi mvs' -> firstTextPart mvs'

  where
  isText = \mv -> case (mimeType $ mime_val_type mv) of
    Text text -> True
    _         -> False

getHeaders :: MIMEValue -> [(String,String)]
getHeaders = mime_val_headers

-- | Convert a String to multiple Strings, cropped by the maximum column
--   size if necessary.
formatBody :: String -> Int -> [String]
formatBody body maxColumns = format [] [] body where
  format parsed acc []                     = parsed ++ [acc]
  format parsed acc ('\r':'\n':xs) = format (parsed ++ [acc]) [] xs
  format parsed acc rest@(x:xs) | length acc < maxColumns = format parsed (acc ++ [x]) xs
                                | otherwise               = format (parsed ++ [acc]) "+" rest


-- The following function is a verbatim copy of the unexported function in
-- Codec.MIME.Parse.
-- case in-sensitive lookup of field names or attributes\/parameters.
lookupField' :: String -> [(String,a)] -> Maybe a
lookupField' n ns =
   -- assume that inputs have been mostly normalized already
   -- (i.e., lower-cased), but should the lookup fail fall back
   -- to a second try where we do normalize before giving up.
  case lookup n ns of
    x@Just{} -> x
    Nothing  ->
      let nl = map toLower n in
      case find (\ (y,_) -> nl == map toLower y) ns of
        Nothing -> Nothing
	Just (_,x)  -> Just x

unwrapField = maybe "" id

lookupField n ns = unwrapField $ lookupField' n ns
