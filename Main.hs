module Main where
import Snapshot (snapshot, Snapshotable(..), subvolumePath, snapshotBasePath)

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

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
      Left str -> putStrLn str
      Right _ -> return ()

data Help =
  Help
  { brief :: String
  , detailed :: String
  }

snapshotHelp :: Help
snapshotHelp =
  Help
  { brief = "Snapshot all configured subvolumes"
  , detailed = "Snapshot all configured subvolumes"
  }

data Verb =
  Verb
  { name :: String
  , help :: Help
  , command :: [String] -> IO ()
  }

verb name help command = Verb { name = name, help = help, command = command }

verbs :: [Verb]
verbs =
  [ verb "snapshot" snapshotHelp snapshotMain
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
