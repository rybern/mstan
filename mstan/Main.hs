module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import Control.Monad

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
execOptions (RunOptions file debugParse maybeOutFile command) = do
    program <- Parsing.readModularProgram file
    when (debugParse == DebugParse) $ do
      putStrLn "============== Parsed program: ==============="
      printModularProgram program
      -- putStrLn "============== Model graph:      ==============="
      -- print $ modelGraph program
      putStrLn "============== Modular tree:     ==============="
      printModularTree program
      putStrLn "============== Results:      ==============="
    result <- Text.unlines <$> execCommand program command
    case maybeOutFile of
      Nothing -> Text.putStr result
      Just f -> Text.writeFile f result

execCommand :: ModularProgram -> ExecCommand -> IO [Text]
execCommand prog (GetNeighbors selection) = return $
  map showSelection . Set.toList $ modelNeighbors prog selection
execCommand prog (GetConcrete selection) = return $
  linesConcreteProgram $ selectModules prog selection
execCommand prog GetMinimumSelection = return $
  [showSelection $ arbitrarySelection prog]
execCommand prog GetModelGraph = do
  let graphviz = modelGraphviz prog
  let graphName = "model_graph"
  filePath  <- publishGraph graphName graphviz
  return [Text.pack filePath]
execCommand prog GetModuleGraph = do
  let moduleGraph = moduleTreeGraphviz prog
  let graphName = "module_graph"
  filePath  <- publishGraph graphName moduleGraph
  return [Text.pack filePath]
execCommand prog GetAllModels = do
  let sels = allSelections prog
  return . map showSelection . Set.toList $ sels
