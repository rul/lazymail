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
import System.FilePath

data Mode = MaildirMode | IndexMode | EmailMode | ComposeMode

data LazymailState = LazymailState {
    mode            :: Mode
  , basePath        :: FilePath
  , screenRows      :: Int
  , screenColumns   :: Int
  , currentRow      :: Int
  , columnPadding   :: Int
  , exitRequested   :: Bool
  , statusBar       :: Bool
  , maildirState    :: MaildirState
  , indexState      :: IndexState
  , composeState    :: ComposeState
  , colorStyle      :: ColorStyle
}

data MaildirState = MaildirState {
    selectedRowMD   :: Int
  , selectedMD      :: String
  , detectedMDs     :: [(FilePath, String)]
}

data IndexState = IndexState {
    selectedRowIn     :: Int
  , selectedEmail     :: Message
  , selectedEmailPath :: FilePath
  , selectedEmails    :: [(FilePath, String)]
  , scrollRowIn       :: Int
  , currentInLen      :: Int
  , scrollBufferIn    :: [(FilePath, String)]
}

data ComposeState = ComposeState {
    composition     :: Maybe String
}

data ColorStyle = ColorStyle {
    baseColorID      :: ColorID
  , selectionColorID :: ColorID
  , statusBarColorID :: ColorID
}

initialState = LazymailState {
    mode          = MaildirMode
  , basePath      = ""
  , screenRows    = 0
  , screenColumns = 0
  , currentRow    = 0
  , columnPadding = 0
  , exitRequested = False
  , statusBar     = True
  , maildirState  = initialMaildirState
  , indexState    = initialIndexState
  , composeState  = initialComposeState
  , colorStyle    = initialColorStyle
}

initialMaildirState = MaildirState {
    selectedRowMD = 0
  , selectedMD    = ""
  , detectedMDs   = []
}

initialIndexState = IndexState {
    selectedRowIn      = 0
  , selectedEmail      = Message [] "Dummy email"
  , selectedEmailPath  = ""
  , selectedEmails     = []
  , scrollRowIn        = 0
  , currentInLen       = 0
  , scrollBufferIn     = []
}

initialComposeState = ComposeState {
    composition = Nothing
}

initialColorStyle = ColorStyle {
    baseColorID      = defaultColorID
  , selectionColorID = defaultColorID
  , statusBarColorID = defaultColorID
}

scrColsAsInteger st = toInteger $ screenColumns st
scrRowsAsInteger st = toInteger $ screenRows st
curRowAsInteger  st = toInteger $ currentRow st
colPadAsInteger  st = toInteger $ columnPadding st


incrementSelectedRow st | (selectedRow st) < limit =
  case (mode st) of
    MaildirMode ->
      let
        sr = (selectedRowMD . maildirState) st
        maildirState' = (maildirState st) { selectedRowMD = sr + 1 }
      in
       st { maildirState = maildirState' }
    IndexMode ->
      let
        sr = (selectedRowIn . indexState) st
        indexState' = (indexState st) { selectedRowIn = sr + 1 }
      in
       st { indexState = indexState' }
    _ -> st
                        | otherwise = st
  where
    scrRows = screenRows st
    limit' = case (mode st) of
      MaildirMode -> (length $ detectedMDs . maildirState $ st ) - 1
      IndexMode   -> if (currentInLen . indexState $ st) < scrRows
                     then (currentInLen . indexState $ st) - 1
                     else scrRows     
    limit = if (statusBar st) && (limit' == scrRows)
            then fromIntegral $ limit' - 2
            else fromIntegral limit'

decrementSelectedRow st | (selectedRow st) > 0 =
  case (mode st) of
    MaildirMode ->
      let
        sr = (selectedRowMD . maildirState) st
        maildirState' = (maildirState st) { selectedRowMD = sr - 1 }
      in
       st { maildirState = maildirState' }
    IndexMode ->
      let
        sr = (selectedRowIn . indexState) st
        indexState' = (indexState st) { selectedRowIn = sr - 1 }
      in
       st { indexState = indexState' }
    _ -> st
                        | otherwise = st

selectedRow st = case (mode st) of
      MaildirMode -> selectedRowMD . maildirState $ st
      IndexMode   -> selectedRowIn . indexState   $ st

