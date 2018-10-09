{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------
-- |
-- Module       : Shush.Data
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Shush.Data (
  -- * Types
    SyncShellError(..)
  , ShellContext(ShellContext)
  , ShellCommandArg(..)
  , ShellCommand(..)
  , InStream(..)
  , OutStream(..)
  , ErrStream(..)
  , ProcessHandle(..)
  , ProcessStatus(..)
  -- * Functions
  , inHandle
  , errHandle
  , outHandle
  , textArg
  , renderSyncShellError
  -- * Lenses and Traversals
  , currentWorkingDirectory
  , environment
  , envvar
  , inputStream
  ) where

import Shush.Data.Environment

import Ultra.Control.Lens ( Lens', Traversal', lens, selected )
import qualified Ultra.Data.Text as T

import System.IO ( Handle )
import System.IO.Error ( IOError )

import qualified System.Process as P

import Preamble

data SyncShellError =
    CallFailed !IOError
  | CommandFailed !Int
  | DirectoryDoesNotExist !T.Text
  | MissingEnvironmentVariables !(NonEmpty T.Text)
    deriving (Show, Eq)

data ShellContext = ShellContext {
    _cwd    :: T.Text
  , _env    :: [(T.Text, T.Text)]
  , _stdin  :: Maybe InStream
  } deriving (Show, Eq)

data ShellCommandArg =
    RawArg T.Text
  | QuoteEnclosed T.Text
  | DoubleQuoteEnclosed T.Text
    deriving (Show, Eq)

textArg :: ShellCommandArg -> T.Text
textArg (RawArg t)              = t
textArg (QuoteEnclosed t)       = T.bracketed "'" "'" t
textArg (DoubleQuoteEnclosed t) = T.bracketed "\"" "\"" t

data ShellCommand = ShellCommand {
    shellEnv        :: !ChildEnvironment
  , shellBin        :: !T.Text
  , shellCmdArgs    :: ![ShellCommandArg]
  }

newtype InStream    = InStream Handle deriving (Show, Eq)
newtype OutStream   = OutStream Handle deriving (Show, Eq)
newtype ErrStream   = ErrStream Handle deriving (Show, Eq)

newtype ProcessHandle = ProcessHandle {
    unProcessHandle :: P.ProcessHandle
  }

-- |
-- You can query the status of a process,
-- It is either still running or has exited with an exit code.
-- 
data ProcessStatus =
    StillRunning
  | ProcessFailed !Int
  | ProcessFinishedCleanly
    deriving (Show, Eq)

inHandle :: InStream -> Handle
inHandle (InStream h) = h

outHandle :: OutStream -> Handle
outHandle (OutStream h) = h

errHandle :: ErrStream -> Handle
errHandle (ErrStream h) = h

renderSyncShellError :: SyncShellError -> T.Text
renderSyncShellError (CallFailed e)                     = "IO Error: " <> (T.pack . show $ e)
renderSyncShellError (CommandFailed code)               = "Received non-zero error code: " <> (T.pack . show $ code)
renderSyncShellError (DirectoryDoesNotExist dir)        = "Directory Doesn't Exist: " <> dir
renderSyncShellError (MissingEnvironmentVariables names) = T.bracketedList "Required Environment Variables were not set: [" "]" ", " . toList $ names

currentWorkingDirectory :: Lens' ShellContext T.Text
currentWorkingDirectory = lens _cwd $ \c cwd' -> c { _cwd = cwd' }

environment :: Lens' ShellContext [(T.Text, T.Text)]
environment = lens _env $ \c e -> c { _env = e }

inputStream :: Lens' ShellContext (Maybe InStream)
inputStream = lens _stdin $ \c sin -> c { _stdin = sin }

-- | Can only be used to modify variables that are already defined.
--
envvar :: T.Text -> Traversal' ShellContext T.Text
envvar n = environment . traverse . selected (== n)


