{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Shush.Control
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Shush.Control (
  -- * Types
    ShT
  -- * Functions
  , hPutCmdLn
  , readStderr
  , readStdout
  , runShT
  , runShTEitherT
  , runSync
  , useInputStream
  , withCwd
  , withInputStream
  , withProcOutPipes
  ) where

import Shush.Data

--import Ultra.Control.Lens ( Lens', Traversal', (.=), both, over, mapped, lens, selected, use, view )
import Ultra.Control.Lens ((.=), both, over, mapped, use)
import Ultra.Control.Monad.Bracket (MonadBracket(..), ebracket, liftSVBracket)
import Ultra.Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Ultra.Control.Monad.Trans.Either (EitherT, left, runEitherT)
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import qualified Ultra.Data.Text.IO as T

import Control.Monad.Morph (MFunctor(..))

import qualified Data.ByteString as BS

import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.IO.Error (IOError)
import System.Process (CmdSpec(..), CreateProcess(..), StdStream(..), createProcess, waitForProcess)

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
  <*> (over (mapped . mapped . both) T.pack $ liftIO getEnvironment)
  <*> pure Nothing

runShT :: (MonadIO m) => ShT m a -> m (Either SyncShellError a)
runShT mx = do
  c <- getShellContext
  fmap fst . flip runStateT c . runEitherT . _runShT $ mx

runShTEitherT :: (MonadIO m) => (SyncShellError -> m a) -> ShT m a -> m a
runShTEitherT err mx = runShT mx >>= either err pure

-- |
-- runs a command synchronously,
-- taking stdin from the current input stream and sending the output (out and err)
-- directly through to the UI.
--
runSync
  :: (MonadIO m, MonadCatch m)
  => ShellCommand
  -> ShT m ()
runSync cmd =
  let
    rawExec :: String
    rawExec = T.unpack . shellBin $ cmd

    rawArgs :: [String]
    rawArgs = fmap (T.unpack . textArg) . shellCmdArgs $ cmd

  in ShT $ do
    inp             <- maybe Inherit (UseHandle . inHandle) <$> use inputStream
    cdr             <- T.unpack <$> use currentWorkingDirectory
    environ'        <- use environment
    let environ     = over (mapped . both) T.unpack environ'
    let proc        = CreateProcess (RawCommand rawExec rawArgs) (pure cdr) (pure environ) inp Inherit Inherit False False False False False False Nothing Nothing False
    (_, _, _, p)    <- (liftIO $ createProcess proc) `catch` handler
    exitcode        <- (liftIO $ waitForProcess p) `catch` handler
    case exitcode of
      ExitFailure c   -> left $ CommandFailed c
      ExitSuccess     -> pure ()

withProcOutPipes
  :: (MonadIO m, MonadBracket m, MonadCatch m)
  => ShellCommand
  -> (OutStream -> ErrStream -> ShT m a)
  -> ShT m a
withProcOutPipes cmd f =
  let
    rawExec :: String
    rawExec = T.unpack . shellBin $ cmd

    rawArgs :: [String]
    rawArgs = fmap (T.unpack . textArg) . shellCmdArgs $ cmd

  in ShT $ do
    inp                     <- maybe Inherit (UseHandle . inHandle) <$> use inputStream
    cdr                     <- T.unpack <$> use currentWorkingDirectory
    environ'                <- use environment
    let environ             = over (mapped . both) T.unpack environ'
    let proc                = CreateProcess (RawCommand rawExec rawArgs) (pure cdr) (pure environ) inp CreatePipe CreatePipe False False False False False False Nothing Nothing False
    let open                = (liftIO $ createProcess proc)
    let close (_, _, _, p)  = liftIO $ waitForProcess p
    flip catch handler . ebracket open close $ \(_, Just outp, Just errp, p) -> do
      x               <- _runShT $ f (OutStream outp) (ErrStream errp)
      -- This will get called twice..
      -- Since it also appears in `close`
      -- But from looking at the implementation for `waitForProcess` i think its ok,
      -- seems to handle the case where its already closed...
      -- At the time of this writing it also seemed to work fine in practice...
      exitcode        <- (liftIO $ waitForProcess p) `catch` handler
      case exitcode of
        ExitFailure c   -> left $ CommandFailed c
        ExitSuccess     -> pure x

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

withInputStream
  :: (MonadIO m, MonadBracket m, MonadCatch m)
  => ShellCommand
  -> (InStream -> ShT m a)
  -> ShT m a
withInputStream cmd f =
  let
    rawExec :: String
    rawExec = T.unpack . shellBin $ cmd

    rawArgs :: [String]
    rawArgs = fmap (T.unpack . textArg) . shellCmdArgs $ cmd

  in ShT $ do
    cdr             <- T.unpack <$> use currentWorkingDirectory
    environ'        <- use environment
    let environ     = over (mapped . both) T.unpack environ'
    let proc        = CreateProcess (RawCommand rawExec rawArgs) (pure cdr) (pure environ) CreatePipe Inherit Inherit False False False False False False Nothing Nothing False
    let open        = (liftIO $ createProcess proc)
    let close       = \(Just inp, _, _, p) -> (liftIO $ hClose inp >> waitForProcess p)
    flip catch handler . ebracket open close $ \(Just inp, _, _, p) -> do
      x               <- _runShT $ f (InStream inp)
      -- This will get called twice..
      -- Since it also appears in `close`
      -- But from looking at the implementation for `waitForProcess` i think its ok,
      -- seems to handle the case where its already closed...
      -- At the time of this writing it also seemed to work fine in practice...
      exitcode        <- (liftIO $ waitForProcess p) `catch` handler
      case exitcode of
        ExitFailure c   -> left $ CommandFailed c
        ExitSuccess     -> pure x

-- |
-- pushes a command into an established pipe
--
hPutCmdLn :: (MonadIO m) => Handle -> ShellCommand -> m ()
hPutCmdLn h = liftIO . T.hPutStrLn h . textCmd

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
