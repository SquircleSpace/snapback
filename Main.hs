module Main where
import GlobalConfig (GlobalConfig, defaultGlobalConfig, configBackupList)
import Send ()
import Snapshot (Snapshotable(..), takeSnapshot, snapshotBasePath)
import SnapshotStore (Subvol(..), loadSnapshotStoreWithFilter, storeSnapshots, lookupSubvolPathToParent, validateSubvol)

import Control.Exception.Base (displayException)
import Control.Monad (forM_)
import Data.Map.Strict (Map, fromList, assocs)
import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)

type VerbCommand = GlobalConfig -> [String] -> IO ()

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

snapshotMain :: VerbCommand
snapshotMain config args = do
  forM_ (configBackupList config) $ \subvolume -> do
    result <- takeSnapshot subvolume
    case result of
      Left str -> putStrLn $ displayException str
      Right _ -> return ()

snapshotVerb :: Verb
snapshotVerb = ("snapshot", (mkVerbInfo snapshotMain) {verbBriefHelp = Just help})
  where help = "Take a snapshot right now"

listMain :: VerbCommand
listMain config args = do
  forM_ (configBackupList config) $ \snapshotable -> do
    putStrLn $ snapshotBasePath snapshotable ++ ":"
    let snapshotStorePath = subvolumePath snapshotable
    snapshotStore <- loadSnapshotStoreWithFilter snapshotStorePath $ \subvol -> do
      case validateSubvol subvol of
        Right _ -> return True
        Left str -> do
          hPutStrLn stderr $ "Ignoring " ++ subvolPath subvol ++ ": " ++ str
          return False
    forM_ (storeSnapshots snapshotStore) $ \subvol -> do
      putStrLn $ subvolName subvol

listVerb :: Verb
listVerb = ("list", (mkVerbInfo listMain) {verbBriefHelp = Just help})
  where help = "List subvolumes in the snapshot directory."

verbs :: VerbTable
verbs = fromList
  [ snapshotVerb
  , listVerb
  ]

usageVerb :: Maybe String -> VerbCommand
usageVerb verbName _ _ = do
  let isBad = case verbName of
                Nothing -> False
                Just _ -> True
  let stream = if isBad then stderr else stdout
  case verbName of
    Nothing -> hPutStrLn stream "You must specify a command to perform"
    Just name -> hPutStrLn stream $ "Unknown command: " ++ name
  hPutStrLn stream "Commands:"
  forM_ (assocs verbs) $ \(name, info) ->
    case verbBriefHelp info of
      Nothing -> hPutStrLn stream name
      Just help -> hPutStrLn stream $ name ++ ": " ++ help
  if isBad then exitFailure else return ()

performVerb :: String -> VerbCommand
performVerb verbName = fromMaybe (usageVerb $ Just verbName) $ command
  where info = Map.lookup verbName verbs
        command = fmap verbCommand info

main :: IO ()
main = do
  args <- getArgs
  let config = globalConfig
  case args of
    [] -> usageVerb Nothing config []
    (verbName : otherArgs) -> performVerb verbName config otherArgs
