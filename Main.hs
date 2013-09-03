{- Main module
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Main (main) where

import System.Environment
import System.Exit
import System.FilePath(takeDirectory)

import Lazymail
import Email
import Maildir
import Screen
import State

parse ["-h"] = usage   >> exit
parse ["--help"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["--version"] = version >> exit
parse _   = run entryPoint

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
