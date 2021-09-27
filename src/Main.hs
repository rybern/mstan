{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Parsing
import Mockup
import ToGraph
import Types
import System.Environment
import System.Clock
import System.Directory
import Control.Concurrent
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import Data.Char
import Data.List
import CLI
import GraphServer

main = parseOptions >>= \case
  Server serverOptions -> runGraphServer serverOptions
  Exec (MStanFile file) maybeOutFile command -> do
    program <- parseModularProgram <$> Text.readFile file
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
  let modelGraph = modelTreeGraph prog
  let graphName = "model_graph"
  filePath  <- publishGraph graphName modelGraph
  return [Text.pack filePath]
execCommand prog GetModuleGraph = do
  let moduleGraph = moduleTreeGraph prog
  let graphName = "module_graph"
  filePath  <- publishGraph graphName moduleGraph
  return [Text.pack filePath]

  -- (cmd, outFile) <- case args of
  --   ["--select", s, "-i", inFile] -> do
  --     contents <- Text.readFile inFile
  --     print contents
  --     let prog = parseModularProgram contents
  --         (Just selections) = parseSelections (Text.pack s)
  --     print prog
  --     return $ (SelectCmd selections prog, Nothing)
  --   ["--select", s, "-i", inFile, "-o", outFile] -> do
  --     contents <- Text.readFile inFile
  --     let prog = parseModularProgram contents
  --         (Just selections) = parseSelections (Text.pack s)
  --     return $ (SelectCmd selections prog, Just outFile)
  --   ["--modelGraph", "-i", inFile, "-o", outFile] -> do
  --     contents <- Text.readFile inFile
  --     let prog = parseModularProgram contents
  --     return $ (ModelGraphCmd prog, Just outFile)
  --   ["--moduleGraph", "-i", inFile, "-o", outFile] -> do
  --     contents <- Text.readFile inFile
  --     let prog = parseModularProgram contents
  --     return $ (ModuleGraphCmd prog, Just outFile)
  -- res <- go cmd
  -- case outFile of
  --   Nothing -> print res
  --   Just file -> do
  --     Text.writeFile file res
