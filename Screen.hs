{- Lazymail interaction with curses.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Screen where

import Control.Monad.Trans(liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Data.List(isPrefixOf)
import UI.NCurses as UI
import Text.ParserCombinators.Parsec.Rfc2822(Message(..))

-- Local imports
import Config
import Lazymail
import Maildir
import Email
import Print
import Rfc1342
import State

type LazymailUpdate = ReaderT LazymailConfig (StateT LazymailState Update)
type LazymailCurses = ReaderT LazymailConfig (StateT LazymailState Curses)

liftCurses = lift . lift
liftUpdate  = lift . lift

entryPoint :: Lazymail ()
entryPoint = do
  st <- get
  maildirs <- liftIO $ getMaildirsRecursively $ basePath st
  let mdState = (maildirState st) { detectedMDs = maildirs  }
  cfg <- ask
  liftIO $ runCurses $ runStateT (runReaderT startCurses cfg) (st { maildirState = mdState })
  return ()

startCurses :: LazymailCurses ()
startCurses = do
  st <- get
  cfg <- ask
  (=<<) put $ liftCurses $ do
    UI.setEcho False
    (rows, cols) <- UI.screenSize
    basColID <- newColorID (fst . baseColor $ cfg) (snd . baseColor $ cfg) 1
    selColID <- newColorID (fst . selectionColor $ cfg) (snd . selectionColor $ cfg) 2
    staColID <- newColorID (fst . statusBarColor $ cfg) (snd . statusBarColor $ cfg) 3
    let style = ColorStyle basColID selColID staColID
    return $ st { screenRows = fromIntegral rows
                , screenColumns = fromIntegral cols
                , colorStyle = style }
  screenLoop

{- This function will loop til the user decides to leave -}
screenLoop :: LazymailCurses ()
screenLoop = do
  w <- liftCurses $ defaultWindow
  st <- get
  cfg <- ask
  liftCurses $ updateWindow w $ do runStateT (runReaderT performUpdate cfg) st
  liftCurses $ render
  handleEvent
  st <- get
  if (not . exitRequested) st
    then screenLoop
    else return ()

performUpdate :: LazymailUpdate ()
performUpdate = do
  st <- get
  liftUpdate $ clearMain (scrRowsAsInteger st) (screenColumns st)
  drawMode (mode st)
  drawStatus

-- | Pattern match on the received mode and draw it in the screen.
drawMode :: Mode -> LazymailUpdate ()
drawMode MaildirMode = do
  st <- get
  let mdState = maildirState st
  drawMaildirHelper $ detectedMDs mdState
--drawMode EmailMode   = drawEmailHelper
--drawMode IndexMode   = drawIndexHelper (selectedEmails st)

-- | Helper function of drawMode
drawMaildirHelper :: [FilePath] -> LazymailUpdate ()
drawMaildirHelper [] = resetCurrentRow
drawMaildirHelper (md:mds) = do
  st <- get
  (=<<) put $ liftUpdate $ do
    moveCursor (curRowAsInteger st) (colPadAsInteger st)
    if (selectedRow st == currentRow st)
    then do
      setColor $ selectionColorID . colorStyle $ st
      drawString $ normalizeLen (screenColumns st) md
      setColor $ baseColorID . colorStyle $ st
      let mdState = (maildirState st) { selectedMD = md }
      return $ st { maildirState = mdState }
    else do
      drawString $ normalizeLen (screenColumns st) md
      return st

  st <- get
  let limit = if statusBar st then (screenRows st) - 1 else screenRows st
  if currentRow st < limit
    then do
      put st { currentRow = (currentRow st) + 1 }
      drawMaildirHelper mds
    else
      resetCurrentRow

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

{-
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

  
-- | Convert a String to multiple Strings, cropped by the maximum column
--   size if necessary.
formatBody :: String -> Int -> [String]
formatBody body maxColumns = format [] [] body where
  format parsed acc []                     = parsed ++ [acc]
  format parsed acc ('\r':'\n':xs) = format (parsed ++ [acc]) [] xs
  format parsed acc rest@(x:xs) | length acc < maxColumns = format parsed (acc ++ [x]) xs
                                | otherwise               = format (parsed ++ [acc]) "+" rest

-}
-- | Draw a status line with the current mode and other stuff
drawStatus  = do
  st <- get
  liftUpdate $ do
    moveCursor ((scrRowsAsInteger st) - 2) 0
    setColor $ statusBarColorID . colorStyle $ st
    drawString . normalizeLen (screenColumns st) . concat $ drawStatusHelper (mode st) st
    setColor $ baseColorID . colorStyle $ st

drawStatusHelper MaildirMode st = ["Maildir listing - "
                                  , "(", show ((+ 1) . selectedRow $ st), "/"
                                  ,  show (length $ detectedMDs . maildirState $ st), ")"]

drawStatusHelper IndexMode st = ["mode: Index - "]
--                                , "(", show ((+ 1) . selectedRow $ st), "/"
--                                ,  show (length $ selectedEmails . indexState $ st), ")"]

drawStatusHelper EmailMode st = ["mode: Email"]

-- | Handle an event
--   TODO: Handle the events in a cleaner way.
handleEvent :: LazymailCurses ()
handleEvent = loop where
  loop = do
    w <- liftCurses $ defaultWindow
    ev <- liftCurses $ getEvent w Nothing
    st <- get
    case ev of
      Nothing  -> loop
      Just ev' -> case ev' of
                    EventCharacter c | c == 'q' || c == 'Q' -> do
                      case (mode st) of
                        IndexMode   -> put $ st { mode = MaildirMode }
                        EmailMode   -> put $ st { mode = IndexMode }
                        MaildirMode -> put $ st { exitRequested = True }

{-                  EventSpecialKey KeyUpArrow  -> put $ decSelectedRow st
                    EventCharacter 'k'          -> put $ decSelectedRow st

                    EventSpecialKey KeyDownArrow -> put $ incSelectedRow st
                    EventCharacter 'j'           -> put $ incSelectedRow st

                    EventSpecialKey KeyRightArrow -> do
                      case (mode st) of
                        IndexMode   -> put $ st { mode = EmailMode }
                        EmailMode   -> return ()
                        MaildirMode -> do
                          selEmails <- liftIO $ getAll . selectedMD $ st
                          return $ st { mode = IndexMode, selectedEmails = selEmails } -}

                    _ ->  loop

resetCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = 0 }