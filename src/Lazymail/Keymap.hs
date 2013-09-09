{- Lazymail default keymap.
 -
 - Copyright 2013 Raúl Benencia <rul@kalgan.cc>
 -
 - Licensed under the GNU GPL version 3 or higher
 -}

module Lazymail.Keymap
       ( defaultGlobalKeymap
       , defaultMaildirKeymap
       , defaultIndexKeymap
       , defaultEmailKeymap
       , defaultComposeKeymap
       , findHandler
       ) where

import UI.NCurses(Event(..), Key(..))

import Lazymail.Types(Keymap, LazymailState(..), Mode(..), LazymailConfig(..))
import Lazymail.Handlers( advanceMode, previousMode, scrollUp, scrollDown
                        , toComposeMode, getFrom, getTo, getSubject, getCc
                        , getBcc, getReplyTo, editEmail, sendEmail
                        )

defaultGlobalKeymap = [ ([EventCharacter '\n', EventCharacter ' ', EventSpecialKey KeyRightArrow], advanceMode)
                      , ([EventCharacter 'q', EventCharacter 'Q'], previousMode)
                      , ([EventSpecialKey KeyUpArrow, EventCharacter 'k'], scrollUp)
                      , ([EventSpecialKey KeyDownArrow, EventCharacter 'j'], scrollDown)
                      , ([EventCharacter 'm'], toComposeMode)
                      ]
defaultMaildirKeymap = []
defaultIndexKeymap   = []
defaultEmailKeymap   = []
defaultComposeKeymap = [ ([EventCharacter 'f'], getFrom)
                       , ([EventCharacter 't'], getTo)
                       , ([EventCharacter 's'], getSubject)
                       , ([EventCharacter 'c'], getCc)
                       , ([EventCharacter 'b'], getBcc)
                       , ([EventCharacter 'r'], getReplyTo)
                       , ([EventCharacter 'e'], editEmail)
                       , ([EventCharacter 'y'], sendEmail)
                       ]

-- | Try to find a keymap for the current mode. If nothing is found, then
--   try looking up in the global keymap.
findHandler st cfg ev = case modeHandler (mode st) ev of
  Nothing  -> globalHandler ev
  h@Just{} -> h
  where
    modeHandler MaildirMode = lookupHandler $ maildirModeKeymap cfg
    modeHandler IndexMode   = lookupHandler $ indexModeKeymap  cfg
    modeHandler EmailMode   = lookupHandler $ emailModeKeymap   cfg
    modeHandler ComposeMode = lookupHandler $ composeModeKeymap cfg

    globalHandler = lookupHandler $ globalKeymap cfg

    lookupHandler [] _   = Nothing
    lookupHandler (km:kms) ev
      | elem ev (fst km) = Just $ snd km
      | otherwise        = lookupHandler kms ev