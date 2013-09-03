{- Lazymail interaction with curses.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 - This code is in an urgent need of a big refactoring.
 -}

module Screen where

import Codec.MIME.Type(MIMEValue(..))
import Control.Monad.Trans(liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char(toUpper)
import Data.List(isPrefixOf)
import System.Exit
import UI.NCurses

-- Local imports
import Config
import qualified Handlers as EH
import Lazymail
import Maildir
import Email(lookupField, getBody, getHeaders, lookupField')
import Print
import Rfc1342
import State
import Types

{- This function is the nexus between Curses and IO -}
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

{- Initial point of screen related functions. Get the number of rows,
 - colors, and start drawing the modes -}
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
    heaColID <- newColorID (fst . headerColor $ cfg) (snd . headerColor $ cfg) 4
    newColID <- newColorID (fst . newEmailColor $ cfg) (snd . newEmailColor $ cfg) 5
    let style = ColorStyle basColID selColID staColID heaColID newColID
    return $ st { screenRows = fromIntegral $ rows - 1
                , screenColumns = fromIntegral $ cols
                , colorStyle = style }
  resetScrollBuffer
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

{- Perform the screen update, by cleaning it first. -}
performUpdate :: LazymailUpdate LazymailState
performUpdate = do
  st <- get
  liftUpdate $ clearMain (scrRowsAsInteger st) (screenColumns st)
  drawMode (mode st)
  drawStatus
  get

{- Pattern match on the received mode and draw it in the screen. -}
drawMode :: Mode -> LazymailUpdate ()
drawMode MaildirMode = get >>= \st -> drawSelectionList $ scrollBufferMD . maildirState $ st
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
        drawSimpleRow st path str
        return st

  st <- get
  let limit = if statusBar st then (screenRows st) - 1 else screenRows st
  if currentRow st < limit
    then do
      incrementCurrentRow
      drawSelectionList mds
    else
      resetCurrentRow

drawSimpleRow st path str | (mode st) == MaildirMode = drawString $ normalizeLen (screenColumns st) str
                          | (mode st) == IndexMode   =
  if isNew path
  then do
    setColor $ newEmailColorID . colorStyle $ st
    drawCroppedString st str
    setColor $ baseColorID . colorStyle $ st
  else
    drawCroppedString st str

{- Empty the whole window. Useful when changing modes. -}
clearMain rows columns = do
  drawEmptyLine 0
  where
    drawEmptyLine currentRow = do
      moveCursor currentRow 0
      drawString $ replicate (columns) ' '
      when (currentRow < rows - 1) $ drawEmptyLine $ currentRow + 1

{- Helper function of drawMode -}
drawEmailHelper = do
  drawEmailHeaders

  st <- get
  let est = emailState st
  put $ st { emailState = est { bodyStartRow = (currentRow st ) } }
  let body = getBody $ currentEmail . emailState $ st
  let maxRows = if statusBar st then (scrRowsAsInteger st) - 1 else scrRowsAsInteger st
  liftUpdate $
    drawBody (curRowAsInteger st) (colPadAsInteger st) maxRows $
      drop (scrollRowEm est) $ emailLines est
  resetCurrentRow

{- Draw the email headers -}
drawEmailHeaders = do
  st <- get
  cfg <- ask
  let hs = getHeaders $ currentEmail . emailState $ st
  let parsedHeaders = parseHeaders hs 0 $ headersToShow cfg

  liftUpdate $ do
    setColor $ headerColorID . colorStyle $ st
    drawHeaders st (curRowAsInteger st) parsedHeaders 
    setColor $ baseColorID . colorStyle $ st
  put $ st { currentRow = 1 + (length parsedHeaders) + (currentRow st) }

  where
    parseHeaders _ _ [] = []
    parseHeaders headers row (h:hs)= do
      let field = lookupField' h headers
      case field of
        Nothing -> parseHeaders headers row hs
        Just f -> let p = capitalize h ++ ": " ++ (ppField f)
                  in p:parseHeaders headers (row + 1) hs

    capitalize str = (toUpper . head $ str):(tail str)
    drawHeaders _ _ []      = return ()
    drawHeaders st row (h:hs) = do
      moveCursor row (colPadAsInteger st)
      drawCroppedString st h
      drawHeaders st (row + 1) hs

{- Draw the email body -}
drawBody _ _ _ [] = return ()
drawBody row col maxRows (xs:xss) = do
  moveCursor row col
  drawString xs
  when (row <  maxRows) $ drawBody (row + 1) col maxRows  xss

{- Draw a status line with the current mode and other stuff -}
drawStatus  = do
  st <- get
  liftUpdate $ do
    moveCursor ((scrRowsAsInteger st) - 1) 0
    setColor $ statusBarColorID . colorStyle $ st
    drawCroppedString st $ concat $ drawStatusHelper (mode st) st
    setColor $ baseColorID . colorStyle $ st

{- Status bar string for Maildir mode -}
drawStatusHelper MaildirMode st =
  ["Maildir listing - "
  , "(", show ((selectedRow st) + (scrollRowMD . maildirState $ st) + 1), "/"
  ,  show (length $ detectedMDs . maildirState $ st), ")"]

{- Status bar string for Index mode -}
drawStatusHelper IndexMode st =
  ["mode: Index - "
  , "(", show ((selectedRow st) + (scrollRowIn . indexState $ st) + 1), "/"
  ,  show (currentInLen . indexState $ st), ")"]

{- Status bar string for Email mode -}
drawStatusHelper EmailMode st = ["mode: Email"]

{- Handle an event
 - TODO: Handle the events in a cleaner way. -}
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

{- Reset the current row to the beginning -}
resetCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = 0 }

{- Advance the current row. Useful when drawing modes -}
incrementCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = (currentRow st) + 1 }

{- Put the scroll at the top -}
resetScrollBuffer = do
  st <- get
  case (mode st) of
    MaildirMode -> do
      let mst = (maildirState st) {
            scrollBufferMD = EH.scrollCrop 0 (screenRows st) $ detectedMDs . maildirState $ st }
      put st { maildirState = mst}
    IndexMode -> do
      let ist = (indexState st) {
            scrollBufferIn = EH.formatIndexModeRows st $  EH.scrollCrop 0 (screenRows st) $ selectedEmails . indexState $ st }
      put st { indexState = ist }

drawCroppedString st str = drawString $ normalizeLen (screenColumns st) str

-- The type system complains if I want to use the same function for diferents monads
liftCurses = lift . lift
liftUpdate  = lift . lift
