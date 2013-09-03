{- Lazymail monad.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -
 -}

module Lazymail where

import Control.Monad.Reader
import Control.Monad.State

import Config
import State
import Types

run :: Lazymail a -> IO (a, LazymailState)
run k =
  let config = customConfig
      state  = initialState { basePath = initialPath config }
  in runStateT (runReaderT k config) state