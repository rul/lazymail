{- Event handlers for Lazymail
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Lazymail.Handlers where

import Codec.MIME.Parse(parseMIMEMessage)
import Codec.MIME.Type(MIMEValue(..))
import Control.Monad.Reader
import Control.Monad.State
import Data.List(intercalate, stripPrefix, sort)
import System.Directory(getTemporaryDirectory)
import System.Exit(ExitCode(..))
import System.FilePath(FilePath, takeFileName, dropTrailingPathSeparator, (</>))
import System.IO(openFile, IOMode(..), hClose, hSeek, SeekMode(..), hPutStrLn)
import System.Locale(rfc822DateFormat)
import System.Process(runProcess, waitForProcess)
import System.Random(randomR, getStdGen, setStdGen)
import Data.DateTime(parseDateTime, startOfTime, formatDateTime)
import qualified System.IO.UTF8 as UTF8
import qualified System.IO.Strict as Strict
import UI.NCurses(setEcho)

import Lazymail.Email(lookupField, getBody, formatBody)
import Lazymail.Maildir
import Lazymail.Print
import Lazymail.State
import Lazymail.Types
import Lazymail.Utils(drawNotification)

previousMode :: LazymailCurses ()
previousMode = get >>= \st -> previousMode' (mode st)

previousMode' MaildirMode = (=<<) put $ get >>= \st -> return st { exitRequested = True }
previousMode' EmailMode   = do
  st <- get
  if (triggerUpdateIn . indexState $ st)
  then do
    advanceMode
    solveIndexUpdate
  else put $ st { mode = IndexMode }
previousMode' IndexMode   = do
  st <- get
  let ist = (indexState st) { selectedRowIn = 0, scrollRowIn = 0 }
  put $ st { mode = MaildirMode, indexState = ist }

previousMode' _  =  get >>= \st -> put $ st { mode = MaildirMode}

advanceMode :: LazymailCurses ()
advanceMode = get >>= \st -> advanceMode' (mode st)

advanceMode' IndexMode   = do
  st <- get
  let fp = selectedEmailPath . indexState $ st
  nfp <- if (isNew fp)
         then liftIO $ markAsRead fp
         else return fp
  when (fp /= nfp) triggerIndexUpdate
  st <- get
  msg <- liftIO $ UTF8.readFile nfp
  let email = parseMIMEMessage msg
  let body = getBody $ email
  let el = formatBody body $ screenColumns st
  let est = (emailState st) { currentEmail = email, emailLines = el, scrollRowEm = 0 }
  put $ st { mode = EmailMode, emailState = est }

advanceMode' MaildirMode =  do
  st <- get
  unsortedEmails <- liftIO $ do
    freeOldHandlers st
    let md = (selectedMD . maildirState) $ st
    emails <- getMaildirEmails md
    mapM toEmail emails
  let selectedEmails' = reverse $ sort unsortedEmails
  let scrollRow = scrollRowIn . indexState $ st
  let scrRows = screenRows st
  let indexState' = (indexState st) {
          selectedEmails = selectedEmails'
        , currentInLen   = length selectedEmails'
        , scrollBufferIn = formatIndexModeRows st $ scrollCrop scrollRow scrRows selectedEmails'
        }
  put $ st { mode = IndexMode, indexState = indexState' }

  where
    toEmail fp = do
      handle <- openFile fp ReadMode
      msg <- UTF8.hGetContents handle
      let value = parseMIMEMessage msg
      let headers = mime_val_headers value
      let date = maybe startOfTime id $ parseDateTime rfc822DateFormat $ takeWhile (/= '(') $ lookupField "date" headers
      return (Email value date fp handle)

advanceMode' _  = return ()

toComposeMode :: LazymailCurses ()
toComposeMode = get >>= \st -> put $ st { mode = ComposeMode }

freeOldHandlers st = mapM (hClose . emailHandle) $ selectedEmails . indexState $ st

scrollDown :: LazymailCurses ()
scrollDown = get >>= \st -> scrollDown' (mode st)

-- Boilerplate code
scrollDown' IndexMode = do
  st <- get
  let inSt = indexState st
  let selRow = selectedRowIn inSt
  let topScrollRow = scrollRowIn inSt
  let startScrolling = (div (screenRows st) 4) * 3
  let totalRows = currentInLen inSt

  if selRow > startScrolling && (topScrollRow <= (totalRows - (screenRows st)))
     then do -- Scroll emails
       let scrollRowIn'    = scrollRowIn inSt + 1
       let scrollBufferIn' = formatIndexModeRows st $ scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
     else -- Move the selected row
       put $ incrementSelectedRow st

scrollDown' MaildirMode = do
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

{- Down-scrolling in Email mode -}
scrollDown' EmailMode = do
  st <- get
  let est = emailState st
  let cur = scrollRowEm est
  let scrRows = screenRows st
  let totalRows = length $ emailLines est
  let est' = est { scrollRowEm = (cur + 1) }

  when ((totalRows - scrRows + (bodyStartRow est) - 1) > (scrollRowEm est)) $
    put $ st { emailState = est' }

scrollDown' _ = return ()

scrollUp :: LazymailCurses ()
scrollUp = get >>= \st -> scrollUp' (mode st)

-- More boilerplate code
scrollUp' IndexMode = do
  st <- get
  let inSt = indexState st
  let selRow = selectedRowIn inSt
  let startScrolling = (div (screenRows st) 4)
  let topScrollRow = scrollRowIn inSt
  if topScrollRow > 0 && selRow < startScrolling
     then do
       let scrollRowIn'    = scrollRowIn inSt - 1
       let scrollBufferIn' = formatIndexModeRows st $ scrollCrop scrollRowIn' (screenRows st) $ selectedEmails inSt
       let inSt'           = inSt { scrollRowIn = scrollRowIn', scrollBufferIn = scrollBufferIn' }
       put st { indexState = inSt' }
      else
        put $ decrementSelectedRow st

scrollUp' MaildirMode = do
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

scrollUp' EmailMode = do
  st <- get
  let est = emailState st
  let cur = scrollRowEm est
  let scrRows = screenRows st
  let totalRows = length $ emailLines est
  let est' = est { scrollRowEm = (cur - 1) }

  when (cur > 0) $
    put $ st { emailState = est' }

scrollUp' _ = return ()

incrementSelectedRow st | (selectedRow st) < limit =
  case (mode st) of
    MaildirMode ->
      let
        sr = (selectedRowMD . maildirState) st
        maildirState' = (maildirState st) { selectedRowMD = sr + 1 }
      in
       st { maildirState = maildirState' }
    IndexMode ->
      let
        sr = (selectedRowIn . indexState) st
        indexState' = (indexState st) { selectedRowIn = sr + 1 }
      in
       st { indexState = indexState' }
    _ -> st
                        | otherwise = st
  where
    scrRows = screenRows st
    curInLen = length $ selectedEmails . indexState $ st
    curMDLen = length $ detectedMDs . maildirState $ st
    limit' = case (mode st) of
      MaildirMode -> if curMDLen < scrRows then curMDLen - 1 else scrRows
      IndexMode   -> if curInLen < scrRows then curInLen - 1 else scrRows
    limit = if (statusBar st) && (limit' == scrRows)
            then fromIntegral $ limit' - 2
            else fromIntegral limit'

decrementSelectedRow st | (selectedRow st) > 0 =
  case (mode st) of
    MaildirMode ->
      let
        sr = (selectedRowMD . maildirState) st
        maildirState' = (maildirState st) { selectedRowMD = sr - 1 }
      in
       st { maildirState = maildirState' }
    IndexMode ->
      let
        sr = (selectedRowIn . indexState) st
        indexState' = (indexState st) { selectedRowIn = sr - 1 }
      in
       st { indexState = indexState' }
    _ -> st
                        | otherwise = st


{- Given a list, it returns the elements that will be in the next screen refresh
 - TODO: find a better name -}
scrollCrop top rows xs = take rows $ drop top xs

formatIndexModeRows :: LazymailState -> [Email] -> [(FilePath, String)]
formatIndexModeRows st = map formatRow where
  formatRow e =
    let fp = emailPath e
        email = emailValue e
        hs = mime_val_headers email
        str = normalizeLen (screenColumns st) $ intercalate ppSep $
              [ "[" ++ normalizeLen maxFlags (ppFlags . getFlags $ fp) ++ "]"
              , formatDateTime "%b %d" $ emailDate e
              , normalizeLen fromLen $ ppField $ lookupField "from" hs
              , ppField $ lookupField "subject" hs
              ]
    in (fp, str)

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

triggerIndexUpdate :: LazymailCurses ()
triggerIndexUpdate = do
  st <- get
  let ist = indexState st
  put $ st { indexState = (ist { triggerUpdateIn = True }) }

solveIndexUpdate :: LazymailCurses ()
solveIndexUpdate = do
  st <- get
  let ist = indexState st
  put $ st { indexState = (ist { triggerUpdateIn = False }) }

triggerMaildirUpdate :: LazymailCurses ()
triggerMaildirUpdate = do
  st <- get
  let mst = maildirState st
  put $ st { maildirState = (mst { triggerUpdateMD = True }) }

solveMaildirUpdate :: LazymailCurses ()
solveMaildirUpdate = do
  st <- get
  let mst = maildirState st
  put $ st { maildirState = (mst { triggerUpdateMD = False }) }

getField :: Maybe String -> LazymailCurses () -> LazymailCurses ()
getField pr postActions = do
  st <- get
  let is = initialInputState { inputRequested = True
                             , prompt = pr
                             , postInputActions = postActions}
  put $ st { inputState = is }

updateField :: (ComposeFields -> String -> ComposeFields) -> LazymailCurses ()
updateField f = do
  st <- get
  let value = currentInput . inputState $ st
  let cf = (composeFields . composeState $ st)
  let cs = (composeState st) { composeFields = (f cf value) }
  put $ st { inputState = initialInputState
           , composeState = cs
           }

getFrom :: LazymailCurses ()
getFrom = let postActions = updateField $ \cf val -> cf { fromField = Just val }
          in getField (Just "From: ") postActions

getTo :: LazymailCurses ()
getTo = let postActions = updateField $ \cf val -> cf { toField = Just val }
        in getField (Just "To: ") postActions

getSubject :: LazymailCurses ()
getSubject = let postActions = updateField $ \cf val -> cf { subjectField = Just val }
             in getField (Just "Subject: ") postActions

getCc :: LazymailCurses ()
getCc = let postActions = updateField $ \cf val -> cf { ccField = Just val }
        in getField (Just "Cc: ") postActions

getBcc :: LazymailCurses ()
getBcc = let postActions = updateField $ \cf val -> cf { bccField = Just val }
        in getField (Just "Bcc: ") postActions

getReplyTo :: LazymailCurses ()
getReplyTo = let postActions = updateField $ \cf val -> cf { replyToField = Just val }
             in getField (Just "Reply-To: ") postActions

editEmail :: LazymailCurses ()
editEmail = do
  st <- get
  cfg <- ask
  fp <- getFileName
  exitStatus <- liftIO $ do
    child <- runProcess (textEditor cfg) [fp] Nothing Nothing Nothing Nothing Nothing
    waitForProcess child
  case exitStatus of
    ExitSuccess -> do
      st <- get
      let cs = (composeState st) { bodyReady = True }
      put $ st { composeState = cs }
    _ -> drawNotification "The text editor exited abnormally"

-- | Retrieve current file name. Create a randomized one if its's Nothing.
getFileName :: LazymailCurses FilePath
getFileName = do
  st <- get
  let cs = composeState st
  case bodyFileName cs of
    Just fp -> return fp
    Nothing -> do
      fp <- liftIO $ newFilename
      let cs = (composeState st) { bodyFileName = Just fp }
      put $ st { composeState = cs }
      return fp

  where
    newFilename = do
      tmp <- getTemporaryDirectory
      num <- getRandomNumber
      return $ (tmp </>) $ ("lazymail-" ++ ) $ show num

getRandomNumber :: IO Int
getRandomNumber = do
  r1 <- getStdGen
  let (num, r2) = randomR (100000,999999) r1
  setStdGen r2
  return num

sendEmail :: LazymailCurses ()
sendEmail = do
  st <- get
  cfg <- ask
  let cs = composeState st
  if not . readyToSend $ cs
    then drawNotification $
         "The email is not ready to be sent. Please check that all fields are correct."
    else do
      exitStatus <- liftIO $ do
        emailHandle <- prepareEmail cs
        child <- runProcess (head . sendmailCommand $ cfg)
                            (tail . sendmailCommand $ cfg)
                            Nothing Nothing (Just emailHandle) Nothing Nothing
        e <- waitForProcess child
        hClose emailHandle
        return e
      handleExitStatus exitStatus

  where
    handleExitStatus ExitSuccess = do
      drawNotification $ "The email was successfully sent."
      st <- get
      put $ st { mode = MaildirMode, composeState = initialComposeState }
    handleExitStatus _ = drawNotification $
                         "Could not send the email. Please, check the logs of for your SMTP client."
    prepareEmail cs = do
      let fs = composeFields cs
      let fileName = (maybe "" id $ bodyFileName cs)
      body <- (Strict.hGetContents =<< openFile fileName ReadMode)
      emailHandle <- openFile fileName WriteMode
      hPutStrLn emailHandle $ (unlines . ppComposeFields True $ fs) ++ body
      hClose emailHandle >> openFile fileName ReadMode

readyToSend cs =
  let from = maybe False (\_ -> True) $ fromField . composeFields $ cs
      to   = maybe False (\_ -> True) $ toField . composeFields $ cs
  in all id [from, to, bodyReady cs]
