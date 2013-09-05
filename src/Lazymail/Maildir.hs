{- Utilities for working with Maildir format.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Lazymail.Maildir  where

import Control.Monad.Loops(allM)
import Control.Monad (forM, filterM)
import Data.List(isPrefixOf)
import System.Directory (doesDirectoryExist, getDirectoryContents, renameFile)
import System.FilePath ((</>), takeFileName, takeDirectory, splitDirectories, joinPath)
import System.IO(IOMode(..), hGetContents, openFile)

import Lazymail.Types(Maildir, Flag(..), Flags)

isMaildir :: FilePath -> IO Bool
isMaildir fp = allM doesDirectoryExist [ fp
                                       , fp </> "cur"
                                       , fp </> "new"
                                       , fp </> "tmp"]

getMaildirEmails md = do
  r <- (getReadEmails md)
  n <- (getNewEmails md)
  return $ r ++ n

getReadEmails md = getEmails $ md </> "cur"
getNewEmails  md = getEmails $ md </> "new"

getEmails fp = do
  contents <- getDirectoryContents fp
  return $  map (fp </>) $ filter (`notElem` [".", ".."]) contents

{- | Returns information about specific messages. -}
getMessages :: Maildir -> [FilePath] -> IO [(FilePath, Flags, String)]
getMessages mb list =  do
  messages <- getAll mb
  return $ filter (\(id, f, m) -> id `elem` list) messages
  
{- Given a mail in a Maildir, mark it as read -}
markAsRead :: FilePath -> IO FilePath
markAsRead fp =
  case newPath of
    Nothing -> return fp
    Just path -> do
      renameFile fp path
      return path
  where newPath = 
          if not $ isNew fp
          then Just fp
          else do
            let fil = takeFileName fp
            let dir = takeDirectory fp
            let spl = splitDirectories dir
            case last spl of
              "cur" -> Just $ fp ++ "S"
              "new" -> Just $ (joinPath . init $ spl) </> ("cur" </> (fil ++ "S"))
              _     -> Nothing


-- Based on getRecursiveContents from Real World Haskell
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


{- The following code is an implementation of the Mailbox interface -}
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
getFlags fp = addNew $ map toFlag $ strip fp
  where strip x
          | null x               = []
          | ":2," `isPrefixOf` x = drop 3 x
          | otherwise            = let (discard, analyze) = span (/= ':') fp
                                   in strip analyze
        addNew flags = if elem SEEN flags then flags else (NEW:flags)

isNew :: FilePath -> Bool
isNew fp = elem NEW $ getFlags fp

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
