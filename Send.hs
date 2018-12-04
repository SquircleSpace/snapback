{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Send
  ( SnapshotStoreLocation
  , getStore
  , storeReceive
  , btrfsSend
  , sendUnsyncedSubvols
  ) where

import SnapshotStore (Subvol(..), SnapshotStore, Local(..), Remote(..), compareStores, storePath, loadSnapshotStore)

import Control.Concurrent.Thread (forkIO)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError, runExceptT)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), makeRelative, isRelative)
import System.IO (Handle)
import System.Process (ProcessHandle, StdStream(..), waitForProcess, createProcess, createPipe, proc, std_in, std_out, std_err)

newtype ParentSubvol = ParentSubvol Subvol
  deriving (Eq, Show)

newtype CloneSrcSubvols = CloneSrcSubvols [Subvol]
  deriving (Eq, Show)

waitOrThrow :: (MonadIO m, MonadError String m) => String -> ProcessHandle -> m ()
waitOrThrow processName handle = do
  exitCode <- liftIO $ waitForProcess handle
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> throwError $ processName ++ " exited with code " ++ show code

btrfsSend :: (MonadIO m, MonadError String m) => Maybe ParentSubvol -> CloneSrcSubvols -> [Subvol] -> Handle -> m ()
btrfsSend parent (CloneSrcSubvols cloneSrc) subvols handle = do
  let defaultArgs = ["-e"]
  let sendParentArgs = case parent of
        Just (ParentSubvol subvol) -> ["-p", subvolPath subvol]
        Nothing -> []
  let sendCloneArgs = concatMap (\subvol -> ["-c", subvolPath subvol]) cloneSrc
  when (null subvols) $ do
    throwError "Must send at least one subvol"
  let args = sendParentArgs ++ sendCloneArgs ++ ["--"] ++ map subvolPath subvols
  let sendCommand = (proc "btrfs-send" args) { std_in = NoStream, std_out = UseHandle handle, std_err = Inherit }
  (_, _, _, process) <- liftIO $ createProcess sendCommand
  waitOrThrow "btrfs-send" process

btrfsReceive :: (MonadIO m, MonadError String m) => FilePath -> Handle -> m ()
btrfsReceive path handle = do
  let receiveCommand = (proc "btrfs-receive" ["-e", "--", path]) { std_in = UseHandle handle, std_out = Inherit, std_err = Inherit }
  (_, _, _, process) <- liftIO $ createProcess receiveCommand
  waitOrThrow "btrfs-receive" process

class SnapshotStoreLocation sl where
  getStore :: (MonadIO m, MonadError String m) => sl -> m SnapshotStore
  storeReceive :: (MonadIO m, MonadError String m) => sl -> FilePath -> Handle -> m ()

sendSubvol :: (MonadIO m, MonadError String m) => Local Subvol -> [Local Subvol] -> Handle -> m ()
sendSubvol (Local subvol) commonSubvols handle =
  btrfsSend Nothing (CloneSrcSubvols $ map fromLocal commonSubvols) [subvol] handle

sendUnsyncedSubvols :: (MonadIO m, MonadError String m, SnapshotStoreLocation local, SnapshotStoreLocation remote) => local -> remote -> m ()
sendUnsyncedSubvols localStorePlace remoteStorePlace = let
  send :: (MonadIO m, MonadError String m) => FilePath -> [Local Subvol] -> [Local Subvol] -> m ()
  send _ [] _ = return ()
  send localStorePath (subvol:rest) common = do
    let relativePath = makeRelative localStorePath $ subvolPath $ fromLocal subvol
    unless (isRelative relativePath) $ do
      throwError $ "Subvol " ++ subvolPath (fromLocal subvol) ++ " is not in store " ++ localStorePath
    (readEnd, writeEnd) <- liftIO $ createPipe
    (_, receiveResultGetter) <- liftIO $ forkIO $ do
      runExceptT $ storeReceive remoteStorePlace relativePath readEnd
    sendSubvol subvol common writeEnd
    receiveResult <- liftIO $ receiveResultGetter
    case receiveResult of
      Left exception -> throwError $ show exception
      Right val -> return val
    send localStorePath rest (subvol:common)
  in do
  localStore <- getStore localStorePlace
  remoteStore <- getStore remoteStorePlace
  let (localOnly, common) = compareStores (Local localStore) (Remote remoteStore)
  send (storePath localStore) localOnly $ map fst common

instance SnapshotStoreLocation (Local FilePath) where
  getStore (Local path) = liftIO $ loadSnapshotStore path
  storeReceive (Local storePath) subvolDestination handle = do
    btrfsReceive (storePath </> subvolDestination) handle
