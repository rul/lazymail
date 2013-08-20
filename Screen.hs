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

import Data.List(isPrefixOf)
import UI.NCurses
import Text.ParserCombinators.Parsec.Rfc2822(Message(..))

-- Local imports
import Maildir
import Email
import Print
import Rfc1342
import State

ppBaseRow = 0
ppBaseColumn = 0

--
-- | Main entry point
--                    
entryPoint :: MState -> IO ()
entryPoint st' = do
  maildirs <- getMaildirsRecursively (initPath st')
  putStrLn $ "We could get " ++ (show . length) maildirs ++ " maildirs."
  runCurses $ do
    setEcho False
    (rows, columns) <- screenSize    
    selColID <- newColorID ColorBlack ColorWhite 1    
    let st = st' {
            scrRows = rows  - 1
          , scrColumns = columns - 1
          , selectedColorID = selColID
          , detectedMDs = maildirs }
    screenLoop st
    
screenLoop :: MState -> Curses ()    
screenLoop st = do
  w <- defaultWindow
  updateWindow w $ do
    clearMain (fromIntegral . scrRows $ st) (fromIntegral . scrColumns $ st)
    drawMode (mode st) st
  render
  st' <- handleEvent st
  if (not . exitRequested) st'
    then screenLoop st'
    else return ()
         
--
-- | Handle an event
--  
handleEvent :: MState -> Curses MState
handleEvent st = loop where
  loop = do
    w <- defaultWindow
    ev <- getEvent w Nothing
    case ev of
      Nothing  -> loop
      Just ev' -> case ev' of
                    EventCharacter c | c == 'q' || c == 'Q' -> return $ st { exitRequested = True } 
                    EventSpecialKey KeyUpArrow  -> return $ decSelectedRow st
                    EventCharacter 'k'          -> return $ decSelectedRow st
                    
                    EventSpecialKey KeyDownArrow -> return $ incSelectedRow st
                    EventCharacter 'j'           -> return $ incSelectedRow st
                    
                    _ ->  loop
         
--
-- | Pattern match on the received mode and draw it in the screen.
--                    
drawMode :: Mode -> MState -> Update ()
drawMode MaildirMode st = drawMaildirHelper st (detectedMDs st)
drawMode EmailMode   st = drawEmailHelper st
drawMode IndexMode   st = drawIndexHelper 0 0 (curRow st) (colPadding st) (selectedEmails st)

drawMaildirHelper _ [] = return ()
drawMaildirHelper st (md:mds) = do
  moveCursor (curRow st) (colPadding st)
  if (selectedRow st == curRow st)
     then do
       setColor $ selectedColorID st
       drawString $ normalizeLen (fromIntegral . scrColumns $ st) md
       setColor defaultColorID
     else drawString $ normalizeLen (fromIntegral . scrColumns $ st) md     
  if curRow st < scrRows st
    then drawMaildirHelper (incCurRow st) mds
    else return ()    

drawIndexHelper origRow origColumn rows columns [] = moveCursor 0 0  
drawIndexHelper origRow origColumn rows columns ((fp, _, msg):ts) = do
  moveCursor origRow origColumn
  let fs = getFields $ parseEmail msg
  drawString $ show $ origRow + 1
  drawString $ (ppSep ++) $ ppFlags . getFlags $ fp
  drawString $ (ppSep ++) $ ppIndexNameAddr . getFrom $ fs    
  drawString $ (ppSep ++) $ ppIndexSubject . getSubject $ fs
  if origRow < (rows - 1)
    then drawIndexHelper (origRow + 1) origColumn rows columns ts
    else return ()
  
waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just ev' -> if p ev' then return () else loop

extractParsedData :: Either a b -> b
extractParsedData (Right msg) = msg
--extractParsedData (Left err)  = error err
                       
drawEmailHelper st = do
  let fs = getFields $ selectedEmail st
  let cropWith xs = normalizeLen $ (fromIntegral . scrColumns $ st) - (length xs)
  let row = curRow st
  moveCursor row (colPadding st)
  drawString $ ("From: " ++) $ cropWith "From: " . ppNameAddr . getFrom $ fs
  moveCursor (row + 1) (colPadding st)
  drawString $ ("To: " ++) $ cropWith "To: " . ppNameAddr . getTo $ fs
  moveCursor (row + 2) (colPadding st)
  drawString $ ("Subject: " ++) $ cropWith "Subject: " . ppSubject . getSubject $ fs
  
  let body = getBody $ selectedEmail st
  drawBody (row + 4) (colPadding st) $ formatBody body (fromIntegral . scrColumns $ st)
  where drawBody _ _ [] = return ()
        drawBody row col (xs:xss) = do
          moveCursor row col
          drawString xs
          if row < (scrRows st) then drawBody (row + 1) col xss else return ()
--
-- | Empty the whole window. Useful when changing modes.
--  
clearMain rows columns = do
  drawEmptyLine 0
  where
    drawEmptyLine currentRow = do
      moveCursor currentRow 0
      drawString $ replicate (columns - 1) ' '
      if currentRow < (rows - 1)
         then drawEmptyLine $ currentRow + 1
         else return ()
  
--  
-- | Convert a String to multiple Strings, cropped by the maximum column
-- | size if necessary.
--      
formatBody :: String -> Int -> [String]
formatBody body maxColumns = format [] [] body where
  format parsed acc []                     = parsed ++ [acc]
  format parsed acc ('\r':'\n':xs) = format (parsed ++ [acc]) [] xs
  format parsed acc rest@(x:xs) | length acc < maxColumns = format parsed (acc ++ [x]) xs
                                | otherwise               = format (parsed ++ [acc]) "+" rest
  


-- drawIndex :: Maildir -> IO ()
-- drawIndex md = do
--   emails <- getAll md
--   runCurses $ do
--     setEcho False
--     (rows, columns) <- screenSize
--     w <- defaultWindow
--     updateWindow w $ do
--       clearMain (fromIntegral rows) (fromIntegral columns)
--       drawIndexHelper 0 0 (fromIntegral rows) (fromIntegral columns) emails
--     render
--     waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
--   let (_, _, msg) = head emails 
--   drawEmail $ parseEmail msg
  
-- drawEmail :: Message -> IO ()
-- drawEmail email = do
--   runCurses $ do
--     setEcho False
--     (rows, columns) <- screenSize    
--     w <- defaultWindow
--     updateWindow w $ do
--       clearMain (fromIntegral rows) (fromIntegral columns)      
--       drawEmailHelper ppBaseRow ppBaseColumn (fromIntegral rows - 1) (fromIntegral columns - 1) email
--     render
--     waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
    
-- drawMaildir :: MState -> IO ()   
-- drawMaildir st = do
--   maildirs <- getMaildirsRecursively (initPath st)
--   runCurses $ do
--     setEcho False
--     (rows, columns) <- screenSize    
--     selColID <- newColorID ColorBlack ColorWhite 1    
--     let st' = st {
--             scrRows = rows  - 1
--           , scrColumns = columns - 1
--           , selectedColorID = selColID }    
--     w <- defaultWindow
--     updateWindow w $ do
--       clearMain (fromIntegral rows) (fromIntegral columns)
--       drawMaildirHelper st' maildirs      
--     render
--     waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
  