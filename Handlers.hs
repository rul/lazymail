{- Event handlers for Lazymail
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Handlers where

import Control.Monad.State
import Data.List(stripPrefix)
import System.FilePath(FilePath, takeFileName, dropTrailingPathSeparator)

import Email(parseEmail, getFields, getSubject, getFrom)
import Maildir
import Print
import State
import System.IO(IOMode(..), hGetContents, openFile)
import Types (LazymailCurses)

previousMode :: Mode -> LazymailCurses ()
previousMode MaildirMode = (=<<) put $ get >>= \st -> return st { exitRequested = True }
previousMode EmailMode   = (=<<) put $ get >>= \st -> return st { mode = IndexMode }
previousMode IndexMode   = do
  st <- get
  let ist = (indexState st) { selectedRowIn = 0, scrollRowIn = 0 }  
  put $ st { mode = MaildirMode, indexState = ist }

changeMode :: Mode -> LazymailCurses ()
changeMode EmailMode   = return ()
changeMode IndexMode   = do
  st <- get
  msg <- liftIO $ readFile . selectedEmailPath . indexState $ st
  let ist = (indexState st) { selectedEmail = (parseEmail msg) }
  put $ st { mode = EmailMode, indexState = ist }
  
changeMode MaildirMode =  do
  st <- get
  selectedEmails' <- liftIO $ do
    let md = (selectedMD . maildirState) $ st
    emails <- getMaildirEmails md
    formatIndexModeRows st emails
  let indexState' = (indexState st) {
          selectedEmails = selectedEmails'
        , currentInLen   = length selectedEmails'
        , scrollBufferIn = scrollCrop (scrollRowIn . indexState $ st) (screenRows st)  selectedEmails'
        }
  put $ st { mode = IndexMode, indexState = indexState' }

{- Boilerplate code -}
incSelectedRow IndexMode = do
  st <- get
  let inSt = indexState st
  let selRow = selectedRowIn inSt
  let topScrollRow = scrollRowIn inSt
  let startScrolling = (div (screenRows st) 4) * 3
  let totalRows = currentInLen inSt

  if selRow > startScrolling && (topScrollRow <= (totalRows - (screenRows st)))
     then do -- Scroll emails
       let scrollRowIn'    = scrollRowIn inSt + 1
       let scrollBufferIn' = scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
     else -- Move the selected row
       put $ incrementSelectedRow st
       
incSelectedRow MaildirMode = do
  st <- get
  let mdSt = maildirState st
  let selRow = selectedRowMD mdSt
  let topScrollRow = scrollRowMD mdSt
  let startScrolling = (div (screenRows st) 4) * 3
  let totalRows = length $ detectedMDs mdSt

  if selRow > startScrolling && (topScrollRow <= (totalRows - (screenRows st)))
     then do -- Scroll emails
       let scrollRowMD'    = topScrollRow + 1
       let scrollBufferMD' = scrollCrop scrollRowMD' (screenRows st) $ detectedMDs mdSt
       let mdSt'           = mdSt { scrollRowMD = scrollRowMD', scrollBufferMD = scrollBufferMD' }
       put st { maildirState = mdSt' }
     else -- Move the selected row
       put $ incrementSelectedRow st

incSelectedRow _ = (=<<) put $ get >>= \st -> return $ incrementSelectedRow st

{- More boilerplate code -}
decSelectedRow IndexMode = do
  st <- get
  let inSt = indexState st
  let selRow = selectedRowIn inSt      
  let startScrolling = (div (screenRows st) 4)
  let topScrollRow = scrollRowIn inSt      
  if topScrollRow > 0 && selRow < startScrolling
     then do
       let scrollRowIn'    = scrollRowIn inSt - 1
       let scrollBufferIn' = scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
      else
        put $ decrementSelectedRow st
        
decSelectedRow MaildirMode = do
  st <- get
  let mdSt = maildirState st
  let selRow = selectedRowMD mdSt      
  let startScrolling = (div (screenRows st) 4)
  let topScrollRow = scrollRowMD mdSt      
  if topScrollRow > 0 && selRow < startScrolling
     then do
       let scrollRowMD'    = scrollRowMD mdSt - 1
       let scrollBufferMD' = scrollCrop scrollRowMD' (screenRows st) $ detectedMDs mdSt
       let mdSt'           = mdSt { scrollRowMD = scrollRowMD', scrollBufferMD = scrollBufferMD' }
       put st { maildirState = mdSt' }
      else
        put $ decrementSelectedRow st
        
decSelectedRow _ = (=<<) put $ get >>= \st -> return $ decrementSelectedRow st

{- Given a list, it returns the elements that will be in the next screen refresh
 - TODO: find a better name -}
scrollCrop top rows xs = take rows $ drop top xs

formatIndexModeRows st = mapM formatRow where
  formatRow fp = do
    msg <- hGetContents =<< (openFile fp ReadMode)
    let email = parseEmail msg
    let fs = getFields email
    let str = normalizeLen (screenColumns st) . concat $
              [ (ppSep ++) $ ppFlags . getFlags $ fp
              , (ppSep ++) $ ppIndexNameAddr . getFrom $ fs
              , (ppSep ++) $ ppIndexSubject . getSubject $ fs
              ]
    return (fp, str)
    
formatMaildirModeRows st = mapM formatRow where
  formatRow fp = return $ (fp, (concat $ replicate (numPads - 1) pad) ++ name) where
    bp      = basePath st
    str     = case (stripPrefix bp fp) of
                Nothing  -> fp
                Just s    -> s
    name'   = takeFileName . dropTrailingPathSeparator $ str
    name    = takeFileName $ map (\x -> if x `elem` imapSep then '/' else x) name'
    pad     = "  "
    numPads = (length $ filter (== '/') str) + (length $ filter (`elem` imapSep) str)
    imapSep = ['.'] -- IMAP usually separates its directories with dots
