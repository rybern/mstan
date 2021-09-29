{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
-- import qualified Data.Map                      as Map

import Parsing
import ModularStan
import ToGraph
import Types
import CLI
import GraphServer

main :: IO ()
main = do
  options <- parseOptions
  -- putStrLn $ "Executing options: " ++ show options
  execOptions options

execOptions :: RunOptions -> IO ()
execOptions (Server serverOptions) = runGraphServer serverOptions
execOptions (Exec file maybeOutFile command) = do
    program <- Parsing.readModularProgram file
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
