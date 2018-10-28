module Main where
import Control.Exception (bracket, try, IOException)
import Control.Monad (forM_)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), (<.>), splitFileName)
import System.Posix.IO (openFd, closeFd, OpenFileFlags, defaultFileFlags, OpenMode (ReadOnly), exclusive)
import System.Posix.Types (Fd)
import System.Process (CreateProcess, proc, readCreateProcessWithExitCode)

data Snapshotable =
  Snapshotable
  { subvolumePath :: FilePath
  , snapshotBasePath :: FilePath
  }

standardSnapshotable :: FilePath -> Snapshotable
standardSnapshotable path =
  Snapshotable
  { subvolumePath = path
  , snapshotBasePath = path </> ".snapshots"
  }

subvolumesToSnapshot :: [Snapshotable]
subvolumesToSnapshot =
  [
    standardSnapshotable "/media"
  ]

snapshotProcess :: FilePath -> FilePath -> CreateProcess
snapshotProcess sourcePath destinationPath =
  proc "btrfs" ["subvolume", "snapshot", "-r", sourcePath, destinationPath]

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

withLockFile :: FilePath -> IO a -> IO (Either String a)
withLockFile lockFile action = do
  let flags = defaultFileFlags { exclusive = True }
  let open = try $ openFd lockFile ReadOnly (Just 0) flags :: IO (Either IOException Fd)
  let close eitherFd = case eitherFd of
        Right fd -> closeFd fd >> removeFile lockFile
        Left _ -> return ()
  bracket open close $ \eitherFd -> case eitherFd of
    Right _ -> do
      value <- action
      return $ Right value
    Left _ -> return $ Left "Failed to create lockfile"

snapshot :: Snapshotable -> IO (Either String ())
snapshot snapshotable = do
  now <- getCurrentTime
  let destinationPath = snapshotDestination snapshotable now
  let createProcess = snapshotProcess (subvolumePath snapshotable) destinationPath
  let processAction = readCreateProcessWithExitCode createProcess ""
  mkdirForSnapshotDestination destinationPath
  result <- withLockFile (lockfilePathForSnapshot destinationPath) processAction
  case result of
    Left e -> return $ Left e
    Right (ExitSuccess, _, _) -> return $ Right ()
    Right (ExitFailure _, stdout, stderr) -> return $ Left stderr

main :: IO ()
main = do
  forM_ subvolumesToSnapshot $ \subvolume -> do
    result <- snapshot subvolume
    case result of
      Left str -> putStrLn str
      Right _ -> return ()
