{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import Control.Monad
import Data.Maybe
import System.IO

import Parsing
import Printing
import ModuleTree
import ModelGraph
import ConcreteProgram
import Graphviz
import Types
import CLI

import DiagnosticPrinting

main :: IO ()
main = do
  options <- parseOptions
  execOptions options

execOptions :: RunOptions -> IO ()
execOptions (RunOptions file maybeSubgraph debugParse maybeOutFile command) =
    Parsing.readModularProgram file >>= \case
      Left errorReport -> mapM_ (hPutStrLn stderr) (showProgramError errorReport)
      Right program -> do
        let subprogram = maybe program (subgraph program) maybeSubgraph
        when (debugParse == DebugParse) $ do
          putStrLn "============== Parsed program: ==============="
          printModularProgram program
          -- putStrLn "============== Model graph:      ==============="
          -- print $ modelGraph program
          putStrLn "============== Modular tree:   ==============="
          printModularTree program
          when (isJust maybeSubgraph) $ do
            putStrLn "============== Modular subtree:   ==============="
            printModularTree subprogram
          putStrLn "============== Results:        ==============="
        result <- Text.unlines <$> execCommand subprogram command
        case maybeOutFile of
          Nothing -> Text.putStr result
          Just f -> Text.writeFile f result

execCommand :: ModularProgram -> ExecCommand -> IO [Text]
execCommand prog (GetNeighbors selection) = return $
  map showSelection . Set.toList $ modelNeighbors prog selection
execCommand prog (GetConcrete selection) = return $
  linesConcreteProgram $ selectModules prog selection
execCommand prog GetMinimumSelection = return $
  [showSelection $ firstSelection prog]
execCommand prog GetModelGraph = do
  let graphviz = modelGraphviz (modelGraph prog)
  let filePath = "model_graph.svg"
  publishGraph filePath graphviz
  return [Text.pack filePath]
execCommand prog GetModuleGraph = do
  let moduleGraph = moduleTreeGraphviz prog
  let filePath = "module_tree.svg"
  publishGraph filePath moduleGraph
  return [Text.pack filePath]
execCommand prog GetAllModels = do
  let sels = allSelections prog
  return . map showSelection . Set.toList $ sels
