{- Utilities for working with Maildir format.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Maildir  where

import Control.Monad.Loops(allM)
import Control.Monad (forM, filterM)
import Data.List(isPrefixOf)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO(IOMode(..), hGetContents, openFile)
import Network.Email.Mailbox(Flag(..), Flags)

type Maildir = FilePath

isMaildir :: FilePath -> IO Bool
isMaildir fp = allM doesDirectoryExist [ fp
                                       , fp </> "cur"
                                       , fp </> "new"
                                       , fp </> "tmp"]

listIDs :: Maildir -> IO [FilePath]
listIDs md = getNewIDs md `appendM` getReadIDs md
  where mxs `appendM` mxs' = do
          xs  <- mxs
          xs' <- mxs'
          return (xs ++ xs')

getNewIDs :: Maildir -> IO [FilePath]
getNewIDs md = getIDs (md </> "new")

getReadIDs :: Maildir -> IO [FilePath]
getReadIDs md = getIDs (md </> "cur")

getIDs :: FilePath -> IO [FilePath]
getIDs fp = do
  names <-getDirectoryContents fp
  let properNames = filter (`notElem` [".", ".."]) names
  return $ map (fp </>) properNames

listMessageFlags :: Maildir -> IO [(FilePath, Flags)]
listMessageFlags fp = do
  ids <- (listIDs fp)
  let flags = map getFlags ids
  return (zip ids flags)

getFlags :: FilePath -> Flags
getFlags fp = map toFlag $ strip fp
  where strip x
          | null x               = []
          | ":2," `isPrefixOf` x = drop 3 x
          | otherwise            = let (discard, analyze) = span (/= ':') fp
                                   in strip analyze

toFlag :: Char -> Flag
toFlag c | c == 'S'  = SEEN
         | c == 'A'  = ANSWERED
         | c == 'F'  = FLAGGED
         | c == 'D'  = DRAFT
         | c == 'P'  = FORWARDED
         | c == 'T'  = DELETED
         | otherwise = OTHERFLAG [c]

getAll :: Maildir -> IO [(FilePath, Flags, String)]
getAll fp = do
  ids <- listIDs fp
  msgs <- mapM (\x -> hGetContents =<< openFile x ReadMode) ids
  let flags = map getFlags ids
  return $ zip3 ids flags msgs

{- | Returns information about specific messages. -}
getMessages :: Maildir -> [FilePath] -> IO [(FilePath, Flags, String)]
getMessages mb list =  do
  messages <- getAll mb
  return $ filter (\(id, f, m) -> id `elem` list) messages

--
-- | Based on getRecursiveContents from Real World Haskell
--
getMaildirsRecursively :: FilePath -> IO [Maildir]
getMaildirsRecursively topdir = do
  result <- search topdir
  includeTopDir <- isMaildir topdir
  if includeTopDir
     then return (topdir:result)
     else return result

  where
    search topdir = do
      names <- getDirectoryContents topdir
      let properNames = filter (`notElem` [".", ".."]) names
      paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then do
            result <- search path
            return ([path] ++ result)
          else return []

      filterM isMaildir (concat paths)

-- Temporal code for testing purposes
defaultPath = "/home/rul/mail/linti/INBOX.academic.c.questions"
getFirstEmail = do
  lst <- getAll defaultPath
  let (_, _, msg) = head lst
  return msg
