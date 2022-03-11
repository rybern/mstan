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
import Macros

main :: IO ()
main = do
  options <- parseOptions
  execOptions options

execOptions :: RunOptions -> IO ()
execOptions (RunOptions file debugParse maybeOutFile command) = do
    unexpandedProgram <- Parsing.readModularProgram file

    when (debugParse == DebugParse) $ do
      putStrLn "============== Parsed program: ==============="
      printModularProgram unexpandedProgram
      -- putStrLn "============== Model graph:      ==============="
      -- print $ modelGraph program

    let (program, macroResult) = applyMacros [collectionHoles] unexpandedProgram

    when (debugParse == DebugParse) $ do
      putStrLn "============== Macro expanded program: ======="
      printModularProgram program
      -- putStrLn "============== Model graph:      ==============="
      -- print $ modelGraph program
      putStrLn "============== Modular tree:   ==============="
      printModularTree program
      putStrLn "============== Results:        ==============="
    result <- Text.unlines <$> execCommand program macroResult command
    case maybeOutFile of
      Nothing -> Text.putStr result
      Just f -> Text.writeFile f result

execCommand :: ModularProgram -> MacroResult -> ExecCommand -> IO [Text]
execCommand prog r (GetNeighbors selection) = return $
  map showSelection . Set.toList $ modelNeighbors prog selection
execCommand prog r (GetConcrete selection) = do
  let (Just uxs) = parseSel selection
  let (Just s) = expandSel r uxs
  return $ linesConcreteProgram $ selectModules prog s
execCommand prog r GetMinimumSelection = return $
  [showSelection $ firstSelection prog]
execCommand prog r GetModelGraph = do
  let graphviz = modelGraphviz (modelGraph prog)
  let filePath = "model_graph.svg"
  publishGraph filePath graphviz
  return [Text.pack filePath]
execCommand prog r GetModuleGraph = do
  let moduleGraph = moduleTreeGraphviz prog
  let filePath = "module_tree.svg"
  publishGraph filePath moduleGraph
  return [Text.pack filePath]
execCommand prog r GetAllModels = do
  let sels = allSelections prog
  let collapsedSels = Set.map (contractSel r) sels
  return . map showUnexpandedSelection . Set.toList $ collapsedSels
