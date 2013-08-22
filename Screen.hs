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

module Screen where

import Control.Monad.Trans(liftIO)
import Data.List(isPrefixOf)
import UI.NCurses
import Text.ParserCombinators.Parsec.Rfc2822(Message(..))

-- Local imports
import Maildir
import Email
import Print
import Rfc1342
import State

-- | Main entry point
entryPoint :: MState -> IO ()
entryPoint st' = do
  maildirs <- getMaildirsRecursively (initPath st')
  putStrLn $ "We could get " ++ (show . length) maildirs ++ " maildirs."
  runCurses $ do
    setEcho False
    (rows, columns) <- screenSize    
    selColID <- newColorID ColorBlack ColorWhite 1
    staColID <- newColorID ColorWhite ColorGreen 2
    let st = st' {
            scrRows = rows  - 1
          , scrColumns = columns - 1
          , selectedColorID = selColID
          , statusColorID   = staColID
          , detectedMDs = maildirs }
    screenLoop st
    
-- | This functions will loop til the user decides to leave
screenLoop :: MState -> Curses ()    
screenLoop st = do
  w   <- defaultWindow
  st' <- updateWindow w $ do
    clearMain (scrRowsAsInt st) (scrColsAsInt st)
    st'' <- drawMode (mode st) st
    drawStatus st''
    return st''
  render
  st'' <- handleEvent st'
  if (not . exitRequested) st''
    then screenLoop st''
    else return ()
         
-- | Pattern match on the received mode and draw it in the screen.
drawMode :: Mode -> MState -> Update MState
drawMode MaildirMode st = drawMaildirHelper st (detectedMDs st)
drawMode EmailMode   st = drawEmailHelper st
drawMode IndexMode   st = drawIndexHelper st $ (selectedEmails st)

-- | Helper function of drawMode
drawMaildirHelper st [] = return $ st { curRow = 0 }
drawMaildirHelper st (md:mds) = do
  moveCursor (curRow st) (colPadding st)
  st' <- if (selectedRow st == curRow st)
         then do
           setColor $ selectedColorID st
           drawString $ normalizeLen (scrColsAsInt st) md
           setColor defaultColorID
           return $ st { selectedMD = md } 
         else do
           drawString $ normalizeLen (scrColsAsInt st) md
           return st
           
  let limit = if showStatus st' then (scrRows st') - 1 else scrRows st'
  if curRow st' < limit
    then drawMaildirHelper (incCurRow st') mds
    else return $ st' { curRow = 0 }

-- | Helper function of drawMode
drawIndexHelper st [] = return $ st { curRow = 0 } --moveCursor 0 0 >> return st 
drawIndexHelper st ((fp, _, msg):ts) = do
  moveCursor (curRow st) (colPadding st)
  let email = parseEmail msg
  let fs = getFields email
  let str = normalizeLen (scrColsAsInt st) . concat $
            [ show $ (curRow st) + 1
            , (ppSep ++) $ ppFlags . getFlags $ fp
            , (ppSep ++) $ ppIndexNameAddr . getFrom $ fs    
            , (ppSep ++) $ ppIndexSubject . getSubject $ fs
            ]
  st' <- if (selectedRow st == curRow st)
         then do
           setColor $ selectedColorID st
           drawString str
           setColor defaultColorID
           return $ st { selectedEmail = email } 
         else do
           drawString str
           return st
  if curRow st' < ((scrRows st') - 1)
    then drawIndexHelper (incCurRow st') ts
    else return $ st' { curRow = 0 }
  
-- | Helper function of drawMode
--   TODO: Make helpers functions to draw header and body in a separate way.  
drawEmailHelper st = do
  let fs = getFields $ selectedEmail st
  let cropWith xs = normalizeLen $ (scrColsAsInt st) - (length xs)
  let row = curRow st
  moveCursor row (colPadding st)
  drawString $ ("From: " ++) $ cropWith "From: " . ppNameAddr . getFrom $ fs
  moveCursor (row + 1) (colPadding st)
  drawString $ ("To: " ++) $ cropWith "To: " . ppNameAddr . getTo $ fs
  moveCursor (row + 2) (colPadding st)
  drawString $ ("Subject: " ++) $ cropWith "Subject: " . ppSubject . getSubject $ fs
  
  let body = getBody $ selectedEmail st
  drawBody (row + 4) (colPadding st) $ formatBody body (scrColsAsInt st)
  return st
  where drawBody _ _ [] = return ()
        drawBody row col (xs:xss) = do
          moveCursor row col
          drawString xs
          if row < (scrRows st) then drawBody (row + 1) col xss else return ()

-- | Empty the whole window. Useful when changing modes.
clearMain rows columns = do
  drawEmptyLine 0
  where
    drawEmptyLine currentRow = do
      moveCursor currentRow 0
      drawString $ replicate (columns - 1) ' '
      if currentRow < (rows - 1)
         then drawEmptyLine $ currentRow + 1
         else return ()
  
-- | Convert a String to multiple Strings, cropped by the maximum column
--   size if necessary.
formatBody :: String -> Int -> [String]
formatBody body maxColumns = format [] [] body where
  format parsed acc []                     = parsed ++ [acc]
  format parsed acc ('\r':'\n':xs) = format (parsed ++ [acc]) [] xs
  format parsed acc rest@(x:xs) | length acc < maxColumns = format parsed (acc ++ [x]) xs
                                | otherwise               = format (parsed ++ [acc]) "+" rest
  

-- | Draw a status line with the current mode and other stuff
drawStatus st = do
  moveCursor ((scrRows st) - 1) 0
  setColor $ statusColorID st
  drawString . normalizeLen (scrColsAsInt st) . concat $ drawStatusHelper (mode st) st
  setColor defaultColorID
  
drawStatusHelper MaildirMode st = ["Maildir listing - "
                                  , "(", show ((+ 1) . selectedRowMD $ st), "/"
                                  ,  show (length $ detectedMDs st), ")"]
    
drawStatusHelper IndexMode st = ["mode: Index - "
                                , "(", show ((+ 1) . selectedRowIn $ st), "/"
                                ,  show (length $ selectedEmails st), ")"]

drawStatusHelper EmailMode st = ["mode: Email"]

-- | Handle an event
--   TODO: Handle the events in a cleaner way.  
handleEvent :: MState -> Curses MState
handleEvent st = loop where
  loop = do
    w <- defaultWindow
    ev <- getEvent w Nothing
    case ev of
      Nothing  -> loop
      Just ev' -> case ev' of
                    EventCharacter c | c == 'q' || c == 'Q' -> do
                      case (mode st) of
                        IndexMode   -> return $ st { mode = MaildirMode }
                        EmailMode   -> return $ st { mode = IndexMode }
                        MaildirMode -> return $ st { exitRequested = True }
  
                    EventSpecialKey KeyUpArrow  -> return $ decSelectedRow st
                    EventCharacter 'k'          -> return $ decSelectedRow st
                    
                    EventSpecialKey KeyDownArrow -> return $ incSelectedRow st
                    EventCharacter 'j'           -> return $ incSelectedRow st

                    EventSpecialKey KeyRightArrow -> do
                      case (mode st) of
                        IndexMode   -> return $ st { mode = EmailMode }
                        EmailMode   -> return st 
                        MaildirMode -> do
                          selEmails <-liftIO $ getAll . selectedMD $ st
                          return $ st { mode = IndexMode, selectedEmails = selEmails }
                    
                    _ ->  loop
