{- Lazymail state, and operations on it.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Lazymail.State where

import Codec.MIME.Type(MIMEValue, nullMIMEValue)
import UI.NCurses(ColorID(..), defaultColorID)
import System.FilePath

import Lazymail.Types

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
  , emailState    = initialEmailState
  , inputState    = initialInputState
  , colorStyle    = initialColorStyle
}

initialMaildirState = MaildirState {
    selectedRowMD   = 0
  , selectedMD      = ""
  , detectedMDs     = []
  , scrollRowMD     = 0
  , scrollBufferMD  = []
  , triggerUpdateMD = False
}

initialIndexState = IndexState {
    selectedRowIn      = 0
  , selectedEmailPath  = ""
  , selectedEmails     = []
  , scrollRowIn        = 0
  , currentInLen       = 0
  , scrollBufferIn     = []
  , triggerUpdateIn    = False
}

initialEmailState = EmailState {
    scrollRowEm    = 0
  , bodyStartRow  = 0
  , emailLines     = []
  , currentEmail   = nullMIMEValue
}

initialComposeState = ComposeState {
    composeFields = initialComposeFields
  , bodyFileName  = Nothing
  , bodyReady     = False
}

initialComposeFields = ComposeFields {
    fromField    = Nothing
  , toField      = Nothing
  , ccField      = Nothing
  , bccField     = Nothing
  , subjectField = Nothing
  , replyToField = Nothing
}

initialColorStyle = ColorStyle {
    baseColorID      = defaultColorID
  , selectionColorID = defaultColorID
  , statusBarColorID = defaultColorID
  , headerColorID    = defaultColorID
  , newEmailColorID  = defaultColorID
}

initialInputState    =  InputState {
    inputRequested   = False
  , prompt           = Nothing
  , currentInput     = ""
  , postInputActions = return ()
}

scrColsAsInteger st = toInteger $ screenColumns st
scrRowsAsInteger st = toInteger $ screenRows st
curRowAsInteger  st = toInteger $ currentRow st
colPadAsInteger  st = toInteger $ columnPadding st

selectedRow st = case (mode st) of
      MaildirMode -> selectedRowMD . maildirState $ st
      IndexMode   -> selectedRowIn . indexState   $ st

