{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
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

stringfyImplID :: ImplID -> Text
stringfyImplID Root = "(root)"
stringfyImplID x = "(" <> unSigName (parent x) <> " " <> unImplName (name x) <> ")"

execOptions :: RunOptions -> IO ()
execOptions (RunOptions file debugParse maybeOutFile command) = do
    program <- Parsing.readModularProgram file
    when (debugParse == DebugParse) $ do
      putStrLn "============== Parsed program: ==============="
      printModularProgram program
      -- putStrLn "============== Model graph:      ==============="
      -- print $ modelGraph program
      putStrLn "============== Modular tree:   ==============="
      printModularTree program
      putStrLn "============== Results:        ==============="
    result <- Text.unlines <$> execCommand program command
    case maybeOutFile of
      Nothing -> Text.putStr result
      Just f -> Text.writeFile f result

execCommand :: ModularProgram -> ExecCommand -> IO [Text]
execCommand prog (GetNeighbors selection) = return $
  map showSelection . Set.toList $ modelNeighbors prog selection
execCommand prog (GetConcrete selection) = return $
  linesConcreteProgram $ selectModules prog selection

execCommand prog GetImplMap = do
  return (map (\(impl, sigs) -> Text.intercalate "|" ((stringfyImplID impl) : map unSigName sigs)) (Map.toList (implSigs prog)))

execCommand prog GetHighestModels = do
  -- let highestModels = findHighestModels prog
  -- return highestModels
  let filteredProg = prog { implementations = filter ((/= ImplName "no") . implName) (implementations prog) }
  let sels = allSelections filteredProg
  return . map showSelection . Set.toList $ sels

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
