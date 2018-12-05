module Main where
import GlobalConfig (HasGlobalConfig, GlobalConfig, defaultGlobalConfig, configBackupList, backupList)
import Send ()
import Snapshot (Snapshotable(..), takeSnapshot, snapshotBasePath)
import SnapshotStore (Subvol(..), loadSnapshotStoreWithFilter, storeSnapshots, lookupSubvolPathToParent, validateSubvol)

import Control.Exception.Base (displayException)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Map.Strict (Map, fromList, assocs)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)
import qualified Data.Map.Strict as Map (lookup)

type VerbCommand = [String] -> ReaderT GlobalConfig IO ()

data VerbInfo =
  VerbInfo
  { verbCommand :: VerbCommand
  , verbBriefHelp :: Maybe String
  , verbDetailedHelp :: Maybe String
  }

mkVerbInfo :: VerbCommand -> VerbInfo
mkVerbInfo command =
  VerbInfo
  { verbCommand = command
  , verbBriefHelp = Nothing
  , verbDetailedHelp = Nothing
  }

type VerbTable = Map String VerbInfo
type Verb = (String, VerbInfo)

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

globalConfig :: GlobalConfig
globalConfig = defaultGlobalConfig { configBackupList = subvolumesToSnapshot }

snapshotMain :: (MonadReader env m, HasGlobalConfig env, MonadIO m) => [String] -> m ()
snapshotMain args = do
  theBackupList <- backupList
  forM_ theBackupList $ \subvolume -> do
    result <- liftIO $ runExceptT $ takeSnapshot subvolume
    case result of
      Left str -> liftIO $ putStrLn $ displayException str
      Right _ -> return ()

snapshotVerb :: Verb
snapshotVerb = ("snapshot", (mkVerbInfo snapshotMain) {verbBriefHelp = Just help})
  where help = "Take a snapshot right now"

listMain :: (MonadReader env m, HasGlobalConfig env, MonadIO m) => [String] -> m ()
listMain args = do
  theBackupList <- backupList
  forM_ theBackupList $ \snapshotable -> do
    liftIO $ putStrLn $ snapshotBasePath snapshotable ++ ":"
    let snapshotStorePath = subvolumePath snapshotable
    snapshotStore <- loadSnapshotStoreWithFilter snapshotStorePath $ \subvol -> do
      case validateSubvol subvol of
        Right _ -> return True
        Left str -> do
          liftIO $ hPutStrLn stderr $ "Ignoring " ++ subvolPath subvol ++ ": " ++ str
          return False
    forM_ (storeSnapshots snapshotStore) $ \subvol -> do
      liftIO $ putStrLn $ subvolName subvol

listVerb :: Verb
listVerb = ("list", (mkVerbInfo listMain) {verbBriefHelp = Just help})
  where help = "List subvolumes in the snapshot directory."

helpVerb :: Verb
helpVerb = ("help", (mkVerbInfo helpMain) {verbBriefHelp = Just help})
  where help = "Show this"

helpMain :: (MonadIO m) => [String] -> m ()
helpMain _ = usageVerb Nothing []

verbs :: VerbTable
verbs = fromList
  [ snapshotVerb
  , listVerb
  , helpVerb
  ]

usageVerb :: (MonadIO m) => Maybe String -> [String] -> m ()
usageVerb verbName _ = do
  let isBad = case verbName of
                Nothing -> False
                Just _ -> True
  let stream = if isBad then stderr else stdout
  case verbName of
    Nothing -> liftIO $ hPutStrLn stream "You must specify a command to perform"
    Just name -> liftIO $ hPutStrLn stream $ "Unknown command: " ++ name
  liftIO $ hPutStrLn stream "Commands:"
  forM_ (assocs verbs) $ \(name, info) ->
    case verbBriefHelp info of
      Nothing -> liftIO $ hPutStrLn stream name
      Just help -> liftIO $ hPutStrLn stream $ name ++ ": " ++ help
  if isBad then liftIO $ exitFailure else return ()

performVerb :: String -> [String] -> GlobalConfig -> IO ()
performVerb verbName args config = runReaderT (verb args) config
  where info = Map.lookup verbName verbs
        command = fmap verbCommand info
        verb = fromMaybe (usageVerb $ Just verbName) $ command

main :: IO ()
main = do
  args <- getArgs
  let config = globalConfig
  case args of
    [] -> usageVerb Nothing []
    (verbName : otherArgs) -> do
      performVerb verbName otherArgs config
