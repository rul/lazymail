{- Email accessors.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}
module Email where

import Network.Email.Mailbox(Flag(..), Flags)

import Text.Parsec.Error(ParseError)
import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Rfc2822

data Email = Email { emailPath :: String
                   , parsedEmail :: Message
                   }

parseEmail :: String -> Message
parseEmail msg = unwrapEmail $ parse message "<stdin>" $  fixEol msg

unwrapEmail (Right email) = email
getFields (Message fs _) = fs

-- There is obviously a pattern here. Find a way to narrow it down.
getReturnPath fs      = do { ReturnPath f <- fs; f }
getFrom fs            = do { From f <- fs; f }
getTo fs              = do { To f <- fs; f }
getCc fs              = do { Cc f <- fs; f }
getBcc fs             = do { Bcc f <- fs; f }
getReplyTo fs         = do { ReplyTo f <- fs; f }
getSubject fs         = do { Subject f <- fs; f }
getMessageID fs       = do { MessageID f <- fs; f }
getInReplyTo fs       = do { InReplyTo f <- fs; f }
getReferences fs      = do { References f <- fs; f }
getComments fs        = do { Comments f <- fs; f }
getKeywords fs        = do { Keywords f <- fs; f }
--getDate fs            = do { Date f <- fs; f }
--getResentDate fs      = do { ResentDate f <- fs; f }
getResentFrom fs      = do { ResentFrom f <- fs; f }
--getResentSender fs    = do { ResentSender f <- fs; f }
getResentTo fs        = do { ResentTo f <- fs; f }
getResentCc fs        = do { ResentCc f <- fs; f }
getResentBcc fs       = do { ResentBcc f <- fs; f }
getResentMessageID fs = do { ResentMessageID f <- fs; f }
--getReceived fs        = do { Received f <- fs; f }

getBody (Message _ []) = "Empty body"
getBody (Message _ body) = body

-- | Convert a String to multiple Strings, cropped by the maximum column
--   size if necessary.
formatBody :: String -> Int -> [String]
formatBody body maxColumns = format [] [] body where
  format parsed acc []                     = parsed ++ [acc]
  format parsed acc ('\r':'\n':xs) = format (parsed ++ [acc]) [] xs
  format parsed acc rest@(x:xs) | length acc < maxColumns = format parsed (acc ++ [x]) xs
                                | otherwise               = format (parsed ++ [acc]) "+" rest

-- Make sure all lines are terminated by CRLF.
fixEol :: String -> String
fixEol ('\r':'\n':xs)   = '\r' : '\n' : fixEol xs
fixEol ('\n':xs)        = '\r' : '\n' : fixEol xs
fixEol (x:xs)           = x : fixEol xs
fixEol []               = []

--data DescriptionPP = DescriptionPP {
--   ppOrder :: [String] -> [String]
-- }


-- emailDescription = emailDescriptionWithPP defaultDescriptionPP

-- emailDescriptionWithPP pp