{- Lazymail interaction with curses.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 - This code is in an urgent need of a big refactoring.
 -}

module Lazymail.Screen where

import Codec.MIME.Type ( MIMEValue(..) )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Reader
import Control.Monad.State
import Data.Char ( toUpper, isPrint )
import Data.List ( isPrefixOf )
import System.Exit
import UI.NCurses

-- Local imports
import Codec.Text.Rfc1342
import Lazymail.Config
import qualified Lazymail.Handlers as EH
import Lazymail.Keymap ( findHandler )
import Lazymail.Maildir
import Lazymail.Email ( lookupField, getBody, getHeaders, lookupField' )
import Lazymail.Print
import Lazymail.State
import Lazymail.Types
import Lazymail.Utils ( newDialogWindow, liftCurses
                      , drawCroppedString, drawNotification
                      )

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
    setCursorMode CursorInvisible
    w <- defaultWindow
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
  st <- get
  if (inputRequested . inputState $ st)
     then handleInputRequest else handleEvent
  get >>= \st -> if (not . exitRequested) st
                 then screenLoop
                 else return ()

{- Perform the screen update, by cleaning it first. -}
performUpdate :: LazymailUpdate LazymailState
performUpdate = do
  st <- get
  liftUpdate $ clearMain (baseColorID . colorStyle $ st) (scrRowsAsInteger st) (screenColumns st)
  drawMode (mode st)
  drawStatus
  get

{- Pattern match on the received mode and draw it in the screen. -}
drawMode :: Mode -> LazymailUpdate ()
drawMode MaildirMode = get >>= \st -> drawSelectionList $ scrollBufferMD . maildirState $ st
drawMode IndexMode   = get >>= \st -> drawSelectionList $ scrollBufferIn . indexState $ st
drawMode EmailMode   = drawEmailHelper
drawMode ComposeMode = drawComposeModeHelper

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
clearMain baseCol rows columns = do
  setColor baseCol
  drawEmptyLine 0
  moveCursor 0 0
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

{- Draw the current Compose mode fields -}
drawComposeModeHelper = do
  st <- get
  let cs = composeState st
  let row = curRowAsInteger st
  let col = colPadAsInteger st
  let maxRows = if statusBar st then (scrRowsAsInteger st) - 1 else scrRowsAsInteger st
  liftUpdate $ do
    drawComposeModeFields st row col maxRows $ ppComposeState cs
    moveCursor (maxRows - 1) col

  where
    drawComposeModeFields _ _ _ _ [] = return ()
    drawComposeModeFields st row col maxRows (f:fs) = do
      moveCursor row col
      drawCroppedString st f
      when (row <  maxRows) $ drawComposeModeFields st (row + 1) col maxRows fs

{- Draw a status line with the current mode and other stuff -}
drawStatus  = do
  st <- get
  liftUpdate $ do
    moveCursor (scrRowsAsInteger st) 0
    setColor $ statusBarColorID . colorStyle $ st
    drawString $ normalizeLen (screenColumns st - 1)$ concat $ drawStatusHelper (mode st) st -- Can't write in the last char - ncurses bug
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

{- Status bar string for Compose mode -}
drawStatusHelper ComposeMode st = ["mode: Compose"]

{- Handle an event -}
handleEvent :: LazymailCurses ()
handleEvent = loop where
  loop = do
    st <- get
    cfg <- ask
    w <- liftCurses $ defaultWindow
    ev <- liftCurses $ getEvent w Nothing
    case ev of
      Nothing  -> loop
      Just ev' ->
        case findHandler st cfg ev' of
          Nothing      -> loop
          Just handler -> handler

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

liftUpdate  = lift . lift

-- Input handling functions --
handleInputRequest :: LazymailCurses ()
handleInputRequest = do
  st <- get
  let is = inputState st
  (_, cols, w) <- liftCurses $ newDialogWindow st
  getLineFromWindow w $ fromIntegral cols
  liftCurses $ closeWindow w

getLineFromWindow :: Window -> Int -> LazymailCurses ()
getLineFromWindow w cols = do
  st <- get
  let is = inputState st
  liftCurses $ do
    updateWindow w $ do
      cleanLine
      moveCursor 1 1
      drawString $ (maybe "" id $ prompt is) ++ (currentInput is)
    render
  loopForEvents w
  st <- get
  when (inputRequested . inputState $ st) $ getLineFromWindow w cols

  where
    cleanLine = moveCursor 1 1 >> (drawString $ replicate (cols - 2) ' ')

    loopForEvents w = do
      st <- get
      let is = inputState st
      let ci = currentInput is
      let pr = maybe "" id $ prompt is
      ev <- liftCurses $ getEvent w Nothing
      case ev of
        Nothing  -> loopForEvents w
        Just ev' -> case ev' of
          EventCharacter '\n' -> do
            postInputActions is
            st' <- get
            put $ st' { inputState = (is { inputRequested = False}) }
          EventCharacter c | isPrint c -> do
            let ci' = if length ci == cols - (length pr) - 2 then ci else ci ++ [c]
            put $ st { inputState = (is { currentInput = ci' }) }
          EventSpecialKey KeyBackspace -> do
            let ci' = if null ci then ci else init ci
            put $ st { inputState = (is { currentInput = ci' } ) }
          _ -> loopForEvents w

