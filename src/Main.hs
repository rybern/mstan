module Main where
import Parsing
import Mockup
import ToGraph
import Types
import System.Environment
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--select", s, "-i", inFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
          (Just selections) = parseSelections (Text.pack s)
          stanModel = selectModules prog selections
      mapM_ Text.putStrLn $ linesConcreteProgram stanModel
    ["--select", s, "-i", inFile, "-o", outFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
          (Just selections) = parseSelections (Text.pack s)
          stanModel = selectModules prog selections
      Text.writeFile outFile . Text.unlines $ linesConcreteProgram stanModel
    ["--modelGraph", "-i", inFile, "-o", outFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
          modelGraph = modelTreeGraph prog
      publishGraph outFile modelGraph
    ["--moduleGraph", "-i", inFile, "-o", outFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
          moduleGraph = moduleTreeGraph prog
      publishGraph outFile moduleGraph

