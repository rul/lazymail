{- Lazymail user configuration
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Config(LazymailConfig(..), defaultConfig, customConfig) where

import Data.List(sort, stripPrefix)
import System.Posix.Files(getSymbolicLinkStatus, isSymbolicLink)
import UI.NCurses(Color(..))

data LazymailConfig = LazymailConfig {
    baseColor          :: (Color, Color) -- (foreground, background)
  , selectionColor     :: (Color, Color)
  , statusBarColor     :: (Color, Color)
  , showStatusBar      :: Bool
  , initialPath        :: FilePath
  , filterMaildirsHook :: [FilePath] -> IO [FilePath]
}

defaultConfig = LazymailConfig {
    baseColor          = (ColorWhite, ColorBlack)
  , selectionColor     = (ColorYellow, ColorBlack)
  , statusBarColor     = (ColorYellow, ColorBlack)
  , showStatusBar      = True
  , initialPath        = ""
  , filterMaildirsHook =  \mds -> return mds
}

--
-- | Users should modify customConfig in order to set-up their
-- preferences. In a possible future maybe I'll work in a not-so-crappy
-- config system.
--
--customConfig = defaultConfig { initialPath = "/home/rul/mail/"}

customConfig = defaultConfig { initialPath = "/home/rul/mail/linti"
                             , filterMaildirsHook = filterSymlinks }

filterSymlinks :: [FilePath] -> IO [FilePath]
filterSymlinks [] = return []
filterSymlinks (md:mds) = do
  filtered <- do
    fs <- getSymbolicLinkStatus md
    rest <- filterSymlinks mds
    if isSymbolicLink fs
      then return rest
      else return (md:rest)
  return $ sort filtered