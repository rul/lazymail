{- Main module
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Main (main) where

import Control.Monad.Reader(runReaderT)
import Control.Monad.State(runStateT)
import System.Environment
import System.Exit
import System.FilePath(takeDirectory)

import Lazymail.Config(customConfig)
import Lazymail.Email
import Lazymail.Maildir
import Lazymail.Screen
import Lazymail.State
import Lazymail.Types

parse ["-h"] = usage   >> exit
parse ["--help"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["--version"] = version >> exit
parse _   = run entryPoint

run :: Lazymail a -> IO (a, LazymailState)
run k =
  let config = customConfig
      state  = initialState { basePath = initialPath config }
  in runStateT (runReaderT k config) state

usage   = putStrLn . unlines $ usageText where
  usageText = ["Usage: ./Main [-vh] <maildirs>"
              ,"      where <maildirs> is a directory with Maildirs, or a Maildir itself."
              ,"      Lazymail will recursively search for Maildirs. "]

version = putStrLn "Haskell lazymail 0.0001"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  parse args
  putStrLn "Game over!"
