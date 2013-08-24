{- Lazymail state, and operations on it.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 - 
 -}

module State where

import Text.ParserCombinators.Parsec.Rfc2822(Message, GenericMessage(..))
import UI.NCurses(ColorID(..), defaultColorID)
import Network.Email.Mailbox(Flag(..), Flags)

data Mode = MaildirMode | IndexMode | EmailMode | ComposeMode

data LazymailState = LazymailState {
    mode            :: Mode
  , screenRows      :: Int
  , screenColumns   :: Int
  , currentRow      :: Int
  , columnPadding   :: Int  
  , exitRequested   :: Bool
  , statusBar       :: Bool    
  , maildirState    :: MaildirState  
  , indexState      :: IndexState  
  , composeState    :: ComposeState    
}

data MaildirState = MaildirState {
    selectedRowMD   :: Int
  , selectedMD      :: String
  , detectedMDs     :: [String]
}

data IndexState = IndexState {
    selectedRowIn   :: Int
  , selectedEmail   :: Message  
  , selectedEmails  :: [(String, [Flag], String)]
}

data ComposeState = ComposeState {
    composition     :: Maybe String
}

initialState = LazymailState {
    mode          = MaildirMode
  , screenRows    = 0                   
  , screenColumns = 0
  , currentRow    = 0                    
  , columnPadding = 0
  , exitRequested = False
  , statusBar     = True                  
  , maildirState  = initialMaildirState
  , indexState    = initialIndexState                    
  , composeState  = initialComposeState                    
}                    

initialMaildirState = MaildirState {
    selectedRowMD = 0
  , selectedMD    = ""
  , detectedMDs   = []                  
}
                    
initialIndexState = IndexState {
    selectedRowIn  = 0
  , selectedEmail  = Message [] "Dummy email"
  , selectedEmails = []
}                     

initialComposeState = ComposeState {
    composition = Nothing
}  
    
{- data MState = MState {
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
-}