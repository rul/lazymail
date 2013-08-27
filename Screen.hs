{- Lazymail interaction with curses.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 - This code is in an urgent need of a big refactoring.
 -}

module Screen where

import Control.Monad.Trans(liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Data.List(isPrefixOf)
import System.Exit
import Text.ParserCombinators.Parsec.Rfc2822(Message(..))
import UI.NCurses

-- Local imports
import Config
import qualified Handlers as EH
import Lazymail
import Maildir
import Email
import Print
import Rfc1342
import State
import Types(LazymailCurses, LazymailUpdate)

entryPoint :: Lazymail ()
entryPoint = do
  st <- get
  cfg <- ask
  maildirs <- liftIO $ do
    mds <- getMaildirsRecursively $ basePath st
    (filterMaildirsHook cfg) mds
  formattedMDs <- EH.formatMaildirModeRows st maildirs
  let mdState = (maildirState st) { detectedMDs = formattedMDs }
  liftIO $ runCurses $ runStateT (runReaderT startCurses cfg) (st { maildirState = mdState })
  return ()

startCurses :: LazymailCurses ()
startCurses = do
  st <- get
  cfg <- ask
  (=<<) put $ liftCurses $ do
    setEcho False
    (rows, cols) <- screenSize
    basColID <- newColorID (fst . baseColor $ cfg) (snd . baseColor $ cfg) 1
    selColID <- newColorID (fst . selectionColor $ cfg) (snd . selectionColor $ cfg) 2
    staColID <- newColorID (fst . statusBarColor $ cfg) (snd . statusBarColor $ cfg) 3
    let style = ColorStyle defaultColorID selColID staColID
    return $ st { screenRows = fromIntegral $ rows - 1
                , screenColumns = fromIntegral $ cols
                , colorStyle = style }
  screenLoop

{- This function will loop til the user decides to leave -}
screenLoop :: LazymailCurses ()
screenLoop = do
  w <- liftCurses $ defaultWindow
  cfg <- ask
  get >>=  \st ->
    (liftCurses . (updateWindow w) $ runStateT (runReaderT performUpdate cfg) st) >>= put . snd
  liftCurses $ render
  handleEvent
  get >>= \st -> if (not . exitRequested) st
                 then screenLoop
                 else return ()

performUpdate :: LazymailUpdate LazymailState
performUpdate = do
  st <- get
  liftUpdate $ clearMain (scrRowsAsInteger st) (screenColumns st)
  drawMode (mode st)
  drawStatus
  get

{- Pattern match on the received mode and draw it in the screen. -}
drawMode :: Mode -> LazymailUpdate ()
drawMode MaildirMode = get >>= \st -> drawSelectionList $ detectedMDs . maildirState $ st
drawMode IndexMode   = get >>= \st -> drawSelectionList $ scrollBufferIn . indexState $ st
drawMode EmailMode   = drawEmailHelper

{- Draw a scrollable selection list -}
drawSelectionList [] = resetCurrentRow
drawSelectionList ((path, str):mds) = do
  st <- get
  (=<<) put $ liftUpdate $ do
    moveCursor (curRowAsInteger st) (colPadAsInteger st)
    if (selectedRow st == currentRow st)
      then do
        setColor $ selectionColorID . colorStyle $ st
        drawString $ normalizeLen (screenColumns st) str
        setColor $ baseColorID . colorStyle $ st
        case (mode st) of
          MaildirMode -> do
            let mst = (maildirState st) { selectedMD = path }
            return $ st { maildirState = mst }
          IndexMode   -> do
            let ist = (indexState st) { selectedEmailPath = path }
            return $ st { indexState = ist }
      else do
        drawString $ normalizeLen (screenColumns st) str
        return st

  st <- get
  let limit = if statusBar st then (screenRows st) - 1 else screenRows st
  if currentRow st < limit
    then do
      incrementCurrentRow
      drawSelectionList mds
    else
      resetCurrentRow

{- Empty the whole window. Useful when changing modes. -}
clearMain rows columns = do
  drawEmptyLine 0
  where
    drawEmptyLine currentRow = do
      moveCursor currentRow 0
      drawString $ replicate (columns) ' '
      if currentRow < rows - 1
         then drawEmptyLine $ currentRow + 1
         else return ()

-- | Helper function of drawMode
--   TODO: Make helpers functions to draw header and body in a separate way.
drawEmailHelper = do
  st <- get
  let fs = getFields $ selectedEmail . indexState $ st
  let cropWith xs = normalizeLen $ (screenColumns st) - (length xs)
  let row = curRowAsInteger st
  liftUpdate $ do
    moveCursor row (colPadAsInteger st)
    drawString $ ("From: " ++) $ cropWith "From: " . ppNameAddr . getFrom $ fs
    moveCursor (row + 1) (colPadAsInteger st)
    drawString $ ("To: " ++) $ cropWith "To: " . ppNameAddr . getTo $ fs
    moveCursor (row + 2) (colPadAsInteger st)
    drawString $ ("Subject: " ++) $ cropWith "Subject: " . ppSubject . getSubject $ fs

  let body = getBody $ selectedEmail . indexState $ st
  liftUpdate $ drawBody (row + 4) (colPadAsInteger st) (scrRowsAsInteger st) $ formatBody body (screenColumns st)

  where drawBody _ _ _ [] = return ()
        drawBody row col maxRows (xs:xss) = do
          moveCursor row col
          drawString xs
          if row <  maxRows then drawBody (row + 1) col maxRows  xss else return ()

-- | Convert a String to multiple Strings, cropped by the maximum column
--   size if necessary.
formatBody :: String -> Int -> [String]
formatBody body maxColumns = format [] [] body where
  format parsed acc []                     = parsed ++ [acc]
  format parsed acc ('\r':'\n':xs) = format (parsed ++ [acc]) [] xs
  format parsed acc rest@(x:xs) | length acc < maxColumns = format parsed (acc ++ [x]) xs
                                | otherwise               = format (parsed ++ [acc]) "+" rest


{- Draw a status line with the current mode and other stuff -}
drawStatus  = do
  st <- get
  liftUpdate $ do
    moveCursor ((scrRowsAsInteger st) - 1) 0
    setColor $ statusBarColorID . colorStyle $ st
    drawString . normalizeLen (screenColumns st) . concat $ drawStatusHelper (mode st) st
    setColor $ baseColorID . colorStyle $ st

drawStatusHelper MaildirMode st =
  ["Maildir listing - "
  , "(", show ((+ 1) . selectedRow $ st), "/"
  ,  show (length $ detectedMDs . maildirState $ st), ")"]

drawStatusHelper IndexMode st =
  ["mode: Index - "
  , "(", show ((selectedRow st) + (scrollRowIn . indexState $ st) + 1), "/"
  ,  show (currentInLen . indexState $ st), ")"]

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
      Just ev' ->
        case ev' of
          EventCharacter 'q'            -> EH.previousMode (mode st)

          EventSpecialKey KeyUpArrow    -> EH.decSelectedRow (mode st)
          EventCharacter 'k'            -> EH.decSelectedRow (mode st)

          EventSpecialKey KeyDownArrow  -> EH.incSelectedRow (mode st)
          EventCharacter 'j'            -> EH.incSelectedRow (mode st)

          EventSpecialKey KeyEnter      -> EH.changeMode (mode st)
          EventSpecialKey KeyRightArrow -> EH.changeMode (mode st)

          _ ->  loop

resetCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = 0 }
incrementCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = (currentRow st) + 1 }

liftCurses = lift . lift
liftUpdate  = lift . lift
