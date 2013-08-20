-- This module is part of Lazymail, a Haskell email client.
--
-- Copyright (C) 2013 Raúl Benencia <rul@kalgan.cc>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main where

import System.Environment
import System.Exit
import System.FilePath(takeDirectory)

import Email
import Maildir
import Screen
import State

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [md]   = do
  putStrLn $ "Maildirs directory: " ++ md
  entryPoint $ initState { initPath = md }  
         
parse []= usage >> die

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
