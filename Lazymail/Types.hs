{- Common types of Lazymail
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Lazymail.Types where

import Codec.MIME.Type(MIMEValue(..))
import Control.Monad.Reader(ReaderT)
import Control.Monad.State(StateT)
import Data.DateTime(DateTime)
import System.FilePath(FilePath)
import System.IO(Handle)
import UI.NCurses(Curses, Update, Color(..), ColorID, Event(..))

type LazymailUpdate = ReaderT LazymailConfig (StateT LazymailState Update)
type LazymailCurses = ReaderT LazymailConfig (StateT LazymailState Curses)

{- Lazymail monad is a ReaderT around a StateT with IO at the bottom of the
 - stack.
 -}
type Lazymail = ReaderT LazymailConfig (StateT LazymailState IO)

data LazymailConfig = LazymailConfig {
    baseColor          :: (Color, Color) -- (foreground, background)
  , selectionColor     :: (Color, Color)
  , statusBarColor     :: (Color, Color)
  , headerColor        :: (Color, Color)
  , newEmailColor      :: (Color, Color)
  , showStatusBar      :: Bool
  , initialPath        :: FilePath
  , filterMaildirsHook :: [FilePath] -> IO [FilePath]
  , indexDateFormat    :: String
  , headersToShow      :: [String]
  , globalKeymaps      :: [Keymap]
  , maildirModeKeymap  :: [Keymap]
  , indexModeKeymap    :: [Keymap]
  , emailModeKeymap    :: [Keymap]
  , composeModeKeymap  :: [Keymap]
}

data Email = Email {
    emailValue  :: MIMEValue
  , emailDate   :: DateTime
  , emailPath   :: FilePath
  , emailHandle :: Handle
}

instance Eq Email where
  (Email _ _ fp1 _) == (Email _ _ fp2 _) = fp1 == fp2

instance Ord Email where
  (Email _ d1 _ _) `compare` (Email _ d2 _ _) = d1 `compare` d2

data Mode = MaildirMode | IndexMode | EmailMode | ComposeMode
                                                  deriving (Show, Eq)

type Maildir = FilePath

data Flag = NEW
          | SEEN
          | ANSWERED
          | FLAGGED
          | DELETED
          | DRAFT
          | FORWARDED
          | OTHERFLAG String
            deriving (Eq)

type Flags = [Flag]

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
  , emailState      :: EmailState
  , composeState    :: ComposeState
  , colorStyle      :: ColorStyle
}

data MaildirState = MaildirState {
    selectedRowMD   :: Int
  , selectedMD      :: String
  , detectedMDs     :: [(FilePath, String)]
  , scrollRowMD     :: Int
  , scrollBufferMD  :: [(FilePath, String)]
  , triggerUpdateMD :: Bool 
}

data IndexState = IndexState {
    selectedRowIn     :: Int
  , selectedEmailPath :: FilePath
  , selectedEmails    :: [Email]
  , scrollRowIn       :: Int
  , currentInLen      :: Int
  , scrollBufferIn    :: [(FilePath, String)]
  , triggerUpdateIn   :: Bool
}

data ComposeState = ComposeState {
    composition     :: Maybe String
}

data EmailState = EmailState {
    scrollRowEm    :: Int
  , bodyStartRow   :: Int
  , emailLines     :: [String]
  , currentEmail   :: MIMEValue
}

data ColorStyle = ColorStyle {
    baseColorID      :: ColorID
  , selectionColorID :: ColorID
  , statusBarColorID :: ColorID
  , headerColorID    :: ColorID
  , newEmailColorID  :: ColorID
}

type Keymap = ([Event], LazymailCurses ())