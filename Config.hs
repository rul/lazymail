{- Lazymail user configuration
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 - 
 -}

module Config(LazymailConfig(..), defaultConfig, customConfig) where

import UI.NCurses(Color(..))
import System.FilePath(FilePath)

data LazymailConfig = LazymailConfig {
    baseColor       :: (Color, Color) -- (foreground, background)
  , selectionColor  :: (Color, Color) 
  , statusBarColor  :: (Color, Color)  
  , showStatusBar   :: Bool
  , basePath        :: Maybe FilePath   
}    

defaultConfig = LazymailConfig {
    baseColor      = (ColorWhite, ColorBlack)
  , selectionColor = (ColorBlack, ColorWhite)
  , statusBarColor = (ColorBlack, ColorWhite)
  , showStatusBar  = True                   
  , basePath       = Nothing                   
}

--
-- | Users should modify customConfig in order to set-up their
-- preferences. In a possible future maybe I'll work in a not-so-crappy
-- config system.
--
customConfig = defaultConfig { basePath = Just "/home/rul/mail/kalgan" }