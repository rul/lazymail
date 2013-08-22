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
    selectedRowMD   :: Integer -- Selected row in MaildirMode
  , selectedRowIn   :: Integer -- Selected row in IndexMode
  , mode            :: Mode
  , initPath        :: String
  , scrRows         :: Integer
  , scrColumns      :: Integer
  , curRow          :: Integer
  , colPadding      :: Integer
  , selectedColorID :: ColorID
  , statusColorID   :: ColorID
  , selectedEmail   :: Message
  , selectedEmails  :: [(String, [Flag], String)]
  , selectedMD      :: String
  , detectedMDs     :: [String]
  , exitRequested   :: Bool
  , showStatus      :: Bool
}

initState = MState {
    selectedRowMD   = 0
  , selectedRowIn   = 0
  , mode            = MaildirMode
  , initPath        = ""
  , scrRows         = (-1)
  , scrColumns      = (-1)
  , curRow          = 0
  , colPadding      = 0             
  , selectedColorID = defaultColorID
  , statusColorID   = defaultColorID                      
  , selectedEmail   = Message [] "Dummy email"
  , selectedEmails  = []
  , selectedMD      = ""                    
  , detectedMDs     = []
  , exitRequested   = False
  , showStatus      = True
} 

incCurRow st = st { curRow = (curRow st) + 1 }

incSelectedRow st | (selectedRow st) < limit = case (mode st) of
                                                 MaildirMode -> st { selectedRowMD = (selectedRowMD st) + 1 }
                                                 IndexMode   -> st { selectedRowIn = (selectedRowIn st) + 1 }
                  | otherwise = st
  where
    limit' = case (mode st) of
      MaildirMode -> (length $ detectedMDs st ) - 1
      IndexMode   -> (length $ selectedEmails st) - 1
    limit = if (showStatus st) && (limit' == scrRowsAsInt st)
            then fromIntegral $ limit' - 2
            else fromIntegral limit'

decSelectedRow st | (selectedRow st) > 0 = case (mode st) of
                                             MaildirMode -> st { selectedRowMD = (selectedRowMD st) - 1 }
                                             IndexMode   -> st { selectedRowIn = (selectedRowIn st) - 1 }
                  | otherwise = st
                                         
selectedRow st = case (mode st) of
      MaildirMode -> selectedRowMD st
      IndexMode   -> selectedRowIn st

scrColsAsInt st = fromIntegral $ scrColumns st
scrRowsAsInt st = fromIntegral $ scrRows st