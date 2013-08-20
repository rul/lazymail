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


