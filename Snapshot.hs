{-# LANGUAGE TemplateHaskell #-}

module Snapshot (takeSnapshot, Snapshotable(..)) where

import Control.Exception (bracket, try, IOException)
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
  year </> month </> day </> timePart
  where
    format :: String -> String
    format str = formatTime defaultTimeLocale str time
    year = format "%Y"
    month = format "%m"
    day = format "%d"
    timePart = format "%T"

snapshotDestination :: Snapshotable -> UTCTime -> FilePath
snapshotDestination snapshotable time = snapshotBasePath snapshotable </> subpathForTime time

mkdirForSnapshotDestination :: FilePath -> IO ()
mkdirForSnapshotDestination snapshotDestination = do
  let (containingDir, target) = splitFileName snapshotDestination
  createDirectoryIfMissing True containingDir

lockfilePathForSnapshot :: FilePath -> FilePath
lockfilePathForSnapshot snapshotPath = snapshotPath <.> "lock"

catchIOExceptions :: IO a -> IO (Either IOException a)
catchIOExceptions action = try action

withLockFile :: FilePath -> IO a -> IO a
withLockFile lockFile action = do
  let flags = defaultFileFlags { exclusive = True }
  let open = openFd lockFile ReadOnly (Just 0) flags
  bracket open closeFd $ \_ -> action

takeSnapshot :: Snapshotable -> IO (Either IOException FilePath)
takeSnapshot snapshotable = do
  now <- getCurrentTime
  let destinationPath = snapshotDestination snapshotable now
  catchIOExceptions $ do
    mkdirForSnapshotDestination destinationPath
    withLockFile (lockfilePathForSnapshot destinationPath) $ do
      let path = subvolumePath snapshotable
      snapshot path destinationPath True
      return path
