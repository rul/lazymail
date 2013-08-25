{- Lazymail user configuration
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Config(LazymailConfig(..), defaultConfig, customConfig) where

import Data.List(sort, stripPrefix)
import System.FilePath(FilePath, takeFileName, dropTrailingPathSeparator)
import System.Posix.Files(getSymbolicLinkStatus, isSymbolicLink)
import UI.NCurses(Color(..))

data LazymailConfig = LazymailConfig {
    baseColor          :: (Color, Color) -- (foreground, background)
  , selectionColor     :: (Color, Color)
  , statusBarColor     :: (Color, Color)
  , showStatusBar      :: Bool
  , initialPath        :: FilePath
  , filterMaildirsHook :: [FilePath] -> IO [FilePath]
  , maildirDrawHook    :: String -> String -> String
}

defaultConfig = LazymailConfig {
    baseColor          = (ColorWhite, ColorBlack)
  , selectionColor     = (ColorBlack, ColorWhite)
  , statusBarColor     = (ColorBlack, ColorWhite)
  , showStatusBar      = True
  , initialPath        = ""
  , filterMaildirsHook =  \mds -> return mds
  , maildirDrawHook    = \_ md ->  md
}

--
-- | Users should modify customConfig in order to set-up their
-- preferences. In a possible future maybe I'll work in a not-so-crappy
-- config system.
--
--customConfig = defaultConfig { initialPath = "/home/rul/mail/"}


customConfig = defaultConfig { initialPath = "/home/rul/mail/linti"
                             , maildirDrawHook = indentedShow
                             , filterMaildirsHook = filterSymlinks }

indentedShow :: String -> String -> String
indentedShow bp md =
  let str     = case (stripPrefix bp md) of
                  Nothing  -> md
                  Just s    -> s
      name'   = takeFileName . dropTrailingPathSeparator $ str
      name    = takeFileName $ map (\x -> if x `elem` imapSep then '/' else x) name'
      pad     = "  "
      numPads = (length $ filter (== '/') str) + (length $ filter (`elem` imapSep) str)
      imapSep = ['.'] -- IMAP usually separates its directories with dots
  in (concat $ replicate (numPads - 1) pad) ++ pad ++ name

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