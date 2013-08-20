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

--
-- | The top level application state, and operations on that value.
--
module State where

import Text.ParserCombinators.Parsec.Rfc2822(Message, GenericMessage(..))
import UI.NCurses(ColorID(..), defaultColorID)
import Network.Email.Mailbox(Flag(..), Flags)

data Mode = MaildirMode | IndexMode | EmailMode

data MState = MState {
    selectedRow     :: Integer
  , mode            :: Mode
  , initPath        :: String
  , scrRows         :: Integer
  , scrColumns      :: Integer
  , curRow          :: Integer
  , colPadding      :: Integer
  , selectedColorID :: ColorID
  , selectedEmail   :: Message
  , selectedEmails  :: [(String, [Flag], String)]
  , selectedMD      :: String
  , detectedMDs     :: [String]
  , exitRequested   :: Bool    
}

initState = MState {
    selectedRow     = 0
  , mode            = MaildirMode
  , initPath        = ""
  , scrRows         = (-1)
  , scrColumns      = (-1)
  , curRow          = 0
  , colPadding      = 0             
  , selectedColorID = defaultColorID
  , selectedEmail   = Message [] "Dummy email"
  , selectedEmails  = []
  , selectedMD      = ""                    
  , detectedMDs     = []
  , exitRequested   = False                      
} 

incCurRow st = st { curRow = (curRow st) + 1 }

incSelectedRow st | selectedRow st < fromIntegral limit = st { selectedRow = (selectedRow st) + 1 }
                  | otherwise                           = st
  where
    limit = case (mode st) of
      MaildirMode -> (length $ detectedMDs st ) - 1
      IndexMode   -> (length $ selectedEmails st) - 1
      _           -> fromIntegral $ scrRows st

decSelectedRow st | selectedRow st > 0 = st { selectedRow = (selectedRow st) - 1 }
                  | otherwise          = st