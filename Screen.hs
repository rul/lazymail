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
import System.Exit

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
  cfg <- ask
  maildirs <- liftIO $ do
    mds <- getMaildirsRecursively $ basePath st
    (filterMaildirsHook cfg) mds
  let mdState = (maildirState st) { detectedMDs = maildirs  }
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
drawMode MaildirMode = get >>= \st -> drawMaildirHelper $ detectedMDs . maildirState $ st
drawMode IndexMode   = get >>= \st -> drawIndexHelper $ scrollBufferIn . indexState $ st
drawMode EmailMode   = drawEmailHelper

{- Helper function of drawMode -}
drawMaildirHelper :: [FilePath] -> LazymailUpdate ()
drawMaildirHelper [] = resetCurrentRow
drawMaildirHelper (md:mds) = do
  st <- get
  cfg <- ask
  let ppMd = (maildirDrawHook cfg) (basePath st) md
  liftUpdate $  moveCursor (curRowAsInteger st) (colPadAsInteger st)
  if (selectedRow st == currentRow st)
    then do
      liftUpdate $ do
        setColor $ selectionColorID . colorStyle $ st
        drawString $ normalizeLen (screenColumns st) ppMd
        setColor $ baseColorID . colorStyle $ st
      let maildirState' = (maildirState st) { selectedMD = md }
      put $ st { maildirState = maildirState' }
    else liftUpdate $ drawString $ normalizeLen (screenColumns st) ppMd

  st <- get
  let limit = if statusBar st then (screenRows st) - 1 else screenRows st
  if currentRow st < limit
    then do
      incrementCurrentRow
      drawMaildirHelper mds
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
drawIndexHelper [] = resetCurrentRow
drawIndexHelper ((fp, _, msg):ts) = do
  st <- get
  (=<<) put $ liftUpdate $ do
    moveCursor (curRowAsInteger st) (colPadAsInteger st)
    let email = parseEmail msg
    let fs = getFields email
    let str = normalizeLen (screenColumns st) . concat $
              [ show $ (currentRow st) + (scrollRowIn . indexState $ st) + 1
              , (ppSep ++) $ ppFlags . getFlags $ fp
              , (ppSep ++) $ ppIndexNameAddr . getFrom $ fs
              , (ppSep ++) $ ppIndexSubject . getSubject $ fs
              ]
    if (selectedRow st == currentRow st)
      then do
        setColor $ selectionColorID . colorStyle $ st
        drawString str
        setColor $ baseColorID . colorStyle $ st
        let indexState' = (indexState st) { selectedEmail = email}
        return $ st { indexState = indexState' }
      else do
        drawString str
        return st

  st <- get
  let limit = if statusBar st then (screenRows st) - 1 else screenRows st
  if currentRow st < limit
     then do
       incrementCurrentRow
       drawIndexHelper ts
     else resetCurrentRow

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


-- | Draw a status line with the current mode and other stuff
drawStatus  = do
  st <- get
  liftUpdate $ do
    moveCursor ((scrRowsAsInteger st) - 1) 0
    setColor $ statusBarColorID . colorStyle $ st
    drawString . normalizeLen (screenColumns st) . concat $ drawStatusHelper (mode st) st
    setColor $ baseColorID . colorStyle $ st

drawStatusHelper MaildirMode st = ["Maildir listing - "
                                  , "(", show ((+ 1) . selectedRow $ st), "/"
                                  ,  show (length $ detectedMDs . maildirState $ st), ")"]

drawStatusHelper IndexMode st = ["mode: Index - "
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
      Just ev' -> case ev' of
                    EventCharacter c | c == 'q' || c == 'Q' -> do
                      case (mode st) of
                        IndexMode   -> put $ st { mode = MaildirMode }
                        EmailMode   -> put $ st { mode = IndexMode }
                        MaildirMode -> put $ st { exitRequested = True }

                    EventSpecialKey KeyUpArrow  -> decrementActions (mode st)
                    EventCharacter 'k'          -> decrementActions (mode st)

                    EventSpecialKey KeyDownArrow -> incrementActions (mode st)
                    EventCharacter 'j'           -> incrementActions (mode st)

                    EventSpecialKey KeyRightArrow -> do
                      case (mode st) of
                        IndexMode   -> put $ st { mode = EmailMode }
                        EmailMode   -> return ()
                        MaildirMode -> do
                          selectedEmails' <- liftIO $ do
                            let md = (selectedMD . maildirState) $ st
                            getAll md
                          let indexState' = (indexState st) { selectedEmails = selectedEmails'
                                                            , currentInLen   = length selectedEmails'
                                                            , scrollBufferIn = scrollCrop (scrollRowIn . indexState $ st) (screenRows st)  selectedEmails'
                                                            }
                          put $ st { mode = IndexMode, indexState = indexState' }

                    _ ->  loop

{- Given a list, it returns the elements that will be in the next screen refresh
 - TODO: find a better name -}
scrollCrop top rows xs = take rows $ drop top xs

incrementActions IndexMode = do
  st <- get
  let inSt = indexState st
  if (selectedRowIn inSt) > (div (screenRows st) 2)
     then do
       let scrollRowIn'    = scrollRowIn inSt + 1
       let scrollBufferIn' = scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
     else put $ incrementSelectedRow st
incrementActions _ = (=<<) put $ get >>= \st -> return $ incrementSelectedRow st

decrementActions IndexMode = do
  st <- get
  let inSt = indexState st
  if (scrollRowIn inSt) > 0
     then do
       let scrollRowIn'    = scrollRowIn inSt - 1
       let scrollBufferIn' = scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
     else put $ decrementSelectedRow st
decrementActions _ = (=<<) put $ get >>= \st -> return $ decrementSelectedRow st

resetCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = 0 }
incrementCurrentRow = (=<<) put $ get >>= \st -> return $ st { currentRow = (currentRow st) + 1 }

