# lazymail
========
lazymail is a Haskell written old-school console Mail User Agent, born as a final project for a functional programming course.

Heavily inspired in [lumail](http://lumail.org/), lazymail is a modal client:
* Maildir mode: offers a tree-like view of all the maildirs recursively found from an initial path.
* Index mode: offers a view of all the emails in a selected maildir.
* Email mode: used to read the selected email in Index mode.
* Compose mode: launch your favorite text editor in order to write your emails.

lazymail main features are:
* Tons of bugs
* Configurable key-bindings
* Configurable colors.
* Some user-configurable hooks.
* That's it.

## Installation
A cabal build system will be available in the followings days. For the moment, use the following pseudo-algorithm to compile lazymail:
```cd src/
missingDeps <- ghc -o Main Main.hs
mapM (\dep -> cabal install dep) missingDeps```

## Usage
The first thing you'll want to do is edit `customConfig` function, in **Config.hs** file. This function has some user configuration in order for the client to work. Edit `initialFile` and point it to the root of your maildirs hierarchy. Re-compile the program.

Then use your cursor keys to move between the different modes. Read **Keymap.hs** to discover the rest of the key-bindings. You can even add your own key-bindings in **Config.hs**.
