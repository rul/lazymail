{- Common types of Lazymail
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Types
       (
         LazymailUpdate
       , LazymailCurses
       ) where

import Control.Monad.Reader(ReaderT)
import Control.Monad.State(StateT)
import UI.NCurses(Curses, Update)

import Config (LazymailConfig)
import State (LazymailState)

type LazymailUpdate = ReaderT LazymailConfig (StateT LazymailState Update)
type LazymailCurses = ReaderT LazymailConfig (StateT LazymailState Curses)
