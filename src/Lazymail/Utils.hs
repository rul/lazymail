{- Miscellaneous functions written apart in order to avoid
 - cyclics module imports
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Lazymail.Utils ( newDialogWindow, drawNotification
                      , liftCurses, drawCroppedString
                      ) where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader
import Control.Monad.State
import UI.NCurses

import Lazymail.Print
import Lazymail.Types
import Lazymail.State

newDialogWindow :: LazymailState -> Curses (Integer, Integer, Window)
newDialogWindow st =
  let rows        = 3
      cols st     = 9 * ((scrColsAsInteger st) `div` 10)
      startCol st = 2 * ((scrColsAsInteger st) `div` 20)
      startRow st = (div (scrRowsAsInteger st) 2) - 1
  in do
    w <- newWindow 3 (cols st) (startRow st) (startCol st)
    updateWindow w $ drawBox Nothing Nothing
    render
    return (rows, cols st, w)

drawNotification :: String -> LazymailCurses ()
drawNotification errorMessage = do
  st <- get
  (_, cols, w) <- liftCurses $ newDialogWindow st
  liftCurses $ do
    updateWindow w $ do
      moveCursor 1 1
      drawString errorMessage
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q' || ev == EventCharacter '\n')
    closeWindow w

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

liftCurses = lift . lift

drawCroppedString st str = drawString $ normalizeLen (screenColumns st) str
