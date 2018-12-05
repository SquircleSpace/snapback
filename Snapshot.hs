{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Snapshot (takeSnapshot, Snapshotable(..)) where

import Control.Exception (bracket, try, IOException)
import Control.Monad.Except (MonadError, throwError, runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), (<.>), splitFileName)
import System.Linux.Btrfs (snapshot)
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode (ReadOnly), exclusive)

data Snapshotable =
  Snapshotable
  { subvolumePath :: FilePath
  , snapshotBasePath :: FilePath
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Snapshotable)

subpathForTime :: UTCTime -> FilePath
subpathForTime time =
  format "%Y-%m-%d-%T"
  where
    format :: String -> String
    format str = formatTime defaultTimeLocale str time

snapshotDestination :: Snapshotable -> UTCTime -> FilePath
snapshotDestination snapshotable time = snapshotBasePath snapshotable </> subpathForTime time

mkdirForSnapshotDestination :: MonadIO m => FilePath -> m ()
mkdirForSnapshotDestination snapshotDestination = do
  let (containingDir, target) = splitFileName snapshotDestination
  liftIO $ createDirectoryIfMissing True containingDir

lockfilePathForSnapshot :: FilePath -> FilePath
lockfilePathForSnapshot snapshotPath = snapshotPath <.> "lock"

liftCatchIO :: (MonadError IOException m, MonadIO m) => IO a -> m a
liftCatchIO action =
  liftIO $ try action >>= liftEither

withLockFile :: (MonadError IOException m, MonadIO m) => FilePath -> IO a -> m a
withLockFile lockFile action = do
  let flags = defaultFileFlags { exclusive = True }
  let open = openFd lockFile ReadOnly (Just 0) flags
  liftCatchIO $ bracket open closeFd $ \_ -> action

takeSnapshot :: (MonadIO m, MonadError IOException m) => Snapshotable -> m FilePath
takeSnapshot snapshotable = do
  now <- liftIO $ getCurrentTime
  let destinationPath = snapshotDestination snapshotable now
  mkdirForSnapshotDestination destinationPath
  either <- withLockFile (lockfilePathForSnapshot destinationPath) $ runExceptT $ do
    let path = subvolumePath snapshotable
    liftCatchIO $ snapshot path destinationPath True
    return path
  liftEither either
