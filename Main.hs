module Main where
import Snapshot (takeSnapshot, Snapshotable(..), subvolumePath, snapshotBasePath)
import SnapshotStore (Subvol(..), loadSnapshotStoreWithFilter, storeSnapshots, lookupSubvolPathToParent, validateSubvol)

import Control.Exception.Base (displayException)
import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

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

snapshotMain :: [String] -> IO ()
snapshotMain args = do
  forM_ subvolumesToSnapshot $ \subvolume -> do
    result <- takeSnapshot subvolume
    case result of
      Left str -> putStrLn $ displayException str
      Right _ -> return ()

data Help =
  Help
  { brief :: String
  , detailed :: String
  }

data Verb =
  Verb
  { name :: String
  , help :: Help
  , command :: [String] -> IO ()
  }

verb name help command = Verb { name = name, help = help, command = command }

snapshotHelp :: Help
snapshotHelp =
  Help
  { brief = "Snapshot all configured subvolumes"
  , detailed = "Snapshot all configured subvolumes"
  }

snapshot :: Verb
snapshot = verb "snapshot" snapshotHelp snapshotMain

listSubvolumesMain :: [String] -> IO ()
listSubvolumesMain args = do
  snapshotStore <- loadSnapshotStoreWithFilter "/media/.snapshots" $ \subvol -> do
    case validateSubvol subvol of
      Right _ -> return True
      Left str -> do
        path <- lookupSubvolPathToParent subvol
        hPutStrLn stderr $ "Ignoring " ++ path ++ ": " ++ str
        return False
  forM_ (storeSnapshots snapshotStore) $ \subvol -> do
    path <- lookupSubvolPathToParent subvol
    putStrLn path

listSubvolumesHelp :: Help
listSubvolumesHelp =
  Help
  { brief = "List subvolumes in the snapshot directory."
  , detailed = "List subvolumes in the snapshot directory."
  }

listSubvolumes :: Verb
listSubvolumes = verb "list" listSubvolumesHelp listSubvolumesMain

verbs :: [Verb]
verbs =
  [ snapshot
  , listSubvolumes
  ]

performVerb :: String -> [String] -> IO ()
performVerb verbName args = search verbs
  where search [] = printUsage >> exitFailure
        search (currentVerb : restVerbs) =
          if name currentVerb == verbName
          then command currentVerb args
          else search restVerbs

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: snapback <command> [args..]"
  putStrLn "Valid commands:"
  forM_ verbs $ \verb -> do
    putStr $ name verb
    putStr ": "
    putStrLn $ brief $ help verb

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printUsage >> exitFailure
    (verbName : otherArgs) -> performVerb verbName otherArgs
