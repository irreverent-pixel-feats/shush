{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Shush.Control
-- Copyright    : (C) 2017-2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Shush.Control (
  -- * Types
    ShT
  -- * Functions
  , interruptAndWaitProcess
  , readStderr
  , readStdout
  , runASync
  , runShT
  , runShTEitherT
  , runSync
  , useInputStream
  , withCwd
  , withInputStream
  , withInputStreamASync
  , withProcOutPipes
  , withProcOutPipesASync
  ) where

import Shush.Data
import Shush.Data.Environment

--import Ultra.Control.Lens ( Lens', Traversal', (.=), both, over, mapped, lens, selected, use, view )
import Ultra.Control.Lens ((.=), both, over, mapped, use)
import Ultra.Control.Monad.Bracket (MonadBracket(..), ebracket, liftSVBracket)
import Ultra.Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Ultra.Control.Monad.Trans.Either (EitherT, left, runEitherT)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

import Control.Monad.Morph (MFunctor(..))

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as H
import Data.Validation (Validation(..))

import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import System.IO.Error (IOError)
import System.Posix.Signals (sigINT)
import System.Process (
    CmdSpec(..)
  , CreateProcess(..)
  , StdStream(..)
  , createProcess
  , interruptProcessGroupOf
  , waitForProcess
  )

import Preamble

newtype ShT m a = ShT {
    _runShT :: EitherT SyncShellError (StateT ShellContext m) a
  } deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO)

instance (MonadBracket m) => MonadBracket (ShT m) where
  svbracket = liftSVBracket ShT _runShT svbracket

instance MFunctor ShT where
--hoist :: (forall a. m a -> n a) -> t m b -> t m b
  hoist f = ShT . hoist (hoist f) . _runShT

instance MonadTrans ShT where
--lift :: m a -> t m a
  lift = ShT . lift . lift

getShellContext :: (MonadIO m) => m ShellContext
getShellContext = ShellContext
  <$> over mapped T.pack (liftIO getCurrentDirectory)
  <*> over (mapped . mapped . both) T.pack (liftIO getEnvironment)
  <*> pure Nothing

runShT :: (MonadIO m) => ShT m a -> m (Either SyncShellError a)
runShT mx = do
  c <- getShellContext
  fmap fst . flip runStateT c . runEitherT . _runShT $ mx

runShTEitherT :: (MonadIO m) => (SyncShellError -> m a) -> ShT m a -> m a
runShTEitherT err mx = runShT mx >>= either err pure

rawArgsFromCommand :: ShellCommand -> [String]
rawArgsFromCommand = fmap (T.unpack . textArg) . shellCmdArgs

startProcess
  :: (MonadIO m, MonadCatch m)
  => ShellCommand
  -> (ShellCommand -> StdStream -> String -> [(String, String)] -> CreateProcess)
  -> ShT m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startProcess cmd proc = ShT $ do
    inp             <- maybe Inherit (UseHandle . inHandle) <$> use inputStream
    cdr             <- T.unpack <$> use currentWorkingDirectory
    parentEnv       <- use environment
    let childEnv'   = buildChildEnvironmentFromParent (shellEnv cmd) (H.fromList parentEnv)
    childEnv        <- case childEnv' of
      Success x -> pure . over (mapped . both) T.unpack $ x
      Failure e -> left $ MissingEnvironmentVariables e
    (mi, mo, me, p) <- liftIO (createProcess $ proc cmd inp cdr childEnv) `catch` handler
    pure (mi, mo, me, ProcessHandle p)


runProc :: ShellCommand -> StdStream -> String -> [(String, String)] -> CreateProcess
runProc cmd inp cdr childEnv =
  CreateProcess
    ((RawCommand <$> rawExec <*> rawArgsFromCommand) cmd)
    (pure cdr)
    (pure childEnv)
    inp
    Inherit
    Inherit
    False
    False
    False
    False
    False
    False
    Nothing
    Nothing
    False

-- |
-- runs a command synchronously,
-- taking stdin from the current input stream and sending the output (out and err)
-- directly through to the UI.
--
runSync
  :: (MonadIO m, MonadCatch m)
  => ShellCommand
  -> ShT m ()
runSync cmd = do
    (_, _, _, p) <- startProcess cmd runProc
    ShT $ waitAndTranslateExitCode p

runASync
  :: (MonadIO m, MonadCatch m)
  => ShellCommand
  -> ShT m ProcessHandle
runASync cmd = do
  (_, _, _, p) <- startProcess cmd runProc
  pure p

rawExec :: ShellCommand -> String
rawExec = T.unpack . shellBin

withProcOutPipesProc :: ShellCommand -> StdStream -> String -> [(String, String)] -> CreateProcess
withProcOutPipesProc cmd inp cdr childEnv = CreateProcess
  ((RawCommand <$> rawExec <*> rawArgsFromCommand) cmd)
  (pure cdr)
  (pure childEnv)
  inp
  CreatePipe
  CreatePipe
  False
  False
  False
  False
  False
  False
  Nothing
  Nothing
  False

withProcOutPipes
  :: (MonadIO m, MonadBracket m, MonadCatch m)
  => ShellCommand
  -> (OutStream -> ErrStream -> ShT m a)
  -> ShT m a
withProcOutPipes cmd' f = do
    let open                              = _runShT $ startProcess cmd' withProcOutPipesProc
    let close (_, _, _, ProcessHandle p)  = liftIO $ waitForProcess p
    ShT . flip catch handler . ebracket open close $ \(_, Just outp, Just errp, p) ->
      _runShT (f (OutStream outp) (ErrStream errp)) <* waitAndTranslateExitCode p

withProcOutPipesASync
  :: (MonadIO m, MonadBracket m, MonadCatch m)
  => ShellCommand
  -> ShT m (OutStream, ErrStream, ProcessHandle)
withProcOutPipesASync cmd = do
  (_, Just outp, Just errp, p) <- startProcess cmd withProcOutPipesProc
  pure (OutStream outp, ErrStream errp, p)

withCwd :: (MonadIO m) => T.Text -> ShT m a -> ShT m a
withCwd dir mx = ShT $ do
  oldCwd <- use currentWorkingDirectory
  b <- liftIO . doesDirectoryExist . T.unpack $ dir
  unless b . left $ DirectoryDoesNotExist dir
  currentWorkingDirectory .= dir
  x <- _runShT mx
  currentWorkingDirectory .= oldCwd
  pure x

useInputStream :: (Monad m) => Maybe InStream -> ShT m a -> ShT m a
useInputStream is p = ShT $ do
  oldis <- use inputStream
  inputStream .= is
  x <- _runShT p
  inputStream .= oldis
  pure x

withInputStreamProc :: ShellCommand -> StdStream -> String -> [(String, String)] -> CreateProcess
withInputStreamProc cmd _ cdr childEnv = CreateProcess
  ((RawCommand <$> rawExec <*> rawArgsFromCommand) cmd)
  (pure cdr)
  (pure childEnv)
  CreatePipe
  Inherit
  Inherit
  False
  False
  False
  False
  False
  False
  Nothing
  Nothing
  False

withInputStream
  :: (MonadIO m, MonadBracket m, MonadCatch m)
  => ShellCommand
  -> (InStream -> ShT m a)
  -> ShT m a
withInputStream cmd' f = do
  let open                              = _runShT $ startProcess cmd' withInputStreamProc
  let close (_, _, _, ProcessHandle p)  = liftIO $ waitForProcess p
  ShT . flip catch handler . ebracket open close $ \(Just inp, _, _, p) ->
    -- waitForProcess will get called twice..
    -- Since it also appears in `close`
    -- But from looking at the implementation for `waitForProcess` i think its ok,
    -- seems to handle the case where its already closed...
    -- At the time of this writing it also seemed to work fine in practice...
    _runShT (f $ InStream inp) <* waitAndTranslateExitCode p

withInputStreamASync
  :: (MonadIO m, MonadBracket m, MonadCatch m)
  => ShellCommand
  -> ShT m (InStream, ProcessHandle)
withInputStreamASync cmd' = do
  (Just inp, _, _, p) <-  startProcess cmd' withInputStreamProc
  pure (InStream inp, p)

-- Wait for process to exit and handle the exit code
waitAndTranslateExitCode
  :: (MonadCatch m, MonadIO m)
  => ProcessHandle
  -> EitherT SyncShellError m ()
waitAndTranslateExitCode (ProcessHandle p) =
  liftIO (waitForProcess p) `catch` handler >>= \case
    ExitFailure c   -> left $ CommandFailed c
    ExitSuccess     -> pure ()

interruptAndWaitProcess
  :: (MonadCatch m, MonadIO m)
  => ProcessHandle
  -> ShT m ()
interruptAndWaitProcess (ProcessHandle p) =
  liftIO (interruptProcessGroupOf p *> waitForProcess p) >>= \case
    ExitFailure c -> if c == -(fromIntegral sigINT) then pure () else ShT . left . CommandFailed $ c
    ExitSuccess   -> pure ()

-- runs a command synchronously and returns the stdout output in its entirety as Text
readStdout
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => ShellCommand
  -> ShT m T.Text
readStdout cmd = withProcOutPipes cmd $ \(OutStream h) _ ->
  T.decodeUtf8 <$> liftIO (BS.hGetContents h)

-- runs a command synchronously and returns the stderr output in its entirety as Text
readStderr
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => ShellCommand
  -> ShT m T.Text
readStderr cmd = withProcOutPipes cmd $ \_ (ErrStream h) ->
  T.decodeUtf8 <$> liftIO (BS.hGetContents h)

-- helpers
--
handler :: (Monad m) => IOError -> EitherT SyncShellError m a
handler = left . CallFailed
