{- Event handlers for Lazymail
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Handlers where

import Control.Monad.State

import Maildir
import State
import Types (LazymailCurses)

previousMode :: Mode -> LazymailCurses ()
previousMode IndexMode   = (=<<) put $ get >>= \st -> return st { mode = MaildirMode }
previousMode EmailMode   = (=<<) put $ get >>= \st -> return st { mode = IndexMode }
previousMode MaildirMode = (=<<) put $ get >>= \st -> return st { exitRequested = True }

changeMode IndexMode   = (=<<) put $ get >>= \st -> return st { mode = EmailMode }
changeMode EmailMode   = return ()
changeMode MaildirMode =  do
  st <- get
  selectedEmails' <- liftIO $ do
    let md = (selectedMD . maildirState) $ st
    getMaildirEmails md
  let indexState' = (indexState st) {
          selectedEmails = selectedEmails'
        , currentInLen   = length selectedEmails'
        , scrollBufferIn = scrollCrop (scrollRowIn . indexState $ st) (screenRows st)  selectedEmails'
        }
  put $ st { mode = IndexMode, indexState = indexState' }

incSelectedRow IndexMode = do
  st <- get
  let inSt = indexState st
  if (selectedRowIn inSt) > (div (screenRows st) 2)
     then do
       let scrollRowIn'    = scrollRowIn inSt + 1
       let scrollBufferIn' = scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
     else put $ incrementSelectedRow st
incSelectedRow _ = (=<<) put $ get >>= \st -> return $ incrementSelectedRow st

decSelectedRow IndexMode = do
  st <- get
  let inSt = indexState st
  if (scrollRowIn inSt) > 0
     then do
       let scrollRowIn'    = scrollRowIn inSt - 1
       let scrollBufferIn' = scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
     else put $ decrementSelectedRow st
decSelectedRow _ = (=<<) put $ get >>= \st -> return $ decrementSelectedRow st

{- Given a list, it returns the elements that will be in the next screen refresh
 - TODO: find a better name -}
scrollCrop top rows xs = take rows $ drop top xs
