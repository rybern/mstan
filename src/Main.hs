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
-- import Server
import WSServer

data Command = ModelGraphCmd ModularProgram
             | ModuleGraphCmd ModularProgram
             | SelectCmd (Map SigName ImplName) ModularProgram
             deriving Show

-- main = cli
main = server

-- main = do
--   let inFile = "gq-concatenation.m.stan"
--   contents <- Text.readFile inFile
--   let prog = parseModularProgram contents
--   print prog
--   let selections = Map.fromList [("Dist", "a")]
--   print (moduleTreeGraph prog)
--   print (modelTree prog)
--   res <- go (SelectCmd selections prog)
--   Text.putStr res

server = runServer $ \text -> do
  readCommand text >>= \c -> (print c *> return c) >>= go

cli :: IO ()
cli = do
  args <- getArgs
  (cmd, outFile) <- case args of
    ["--select", s, "-i", inFile] -> do
      contents <- Text.readFile inFile
      print contents
      let prog = parseModularProgram contents
          (Just selections) = parseSelections (Text.pack s)
      print prog
      return $ (SelectCmd selections prog, Nothing)
    ["--select", s, "-i", inFile, "-o", outFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
          (Just selections) = parseSelections (Text.pack s)
      return $ (SelectCmd selections prog, Just outFile)
    ["--modelGraph", "-i", inFile, "-o", outFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
      return $ (ModelGraphCmd prog, Just outFile)
    ["--moduleGraph", "-i", inFile, "-o", outFile] -> do
      contents <- Text.readFile inFile
      let prog = parseModularProgram contents
      return $ (ModuleGraphCmd prog, Just outFile)
  res <- go cmd
  case outFile of
    Nothing -> print res
    Just file -> do
      Text.writeFile file res

readCommand :: Text -> IO Command
readCommand msg = do
  let (cmd:lines) = Text.lines msg
  case Text.strip cmd of
    "model-graph" -> return . ModelGraphCmd . parseModularProgram $ Text.unlines lines
    "module-graph" -> return . ModuleGraphCmd . parseModularProgram $ Text.unlines lines
    "select" -> do
      let (selectionsString:rest) = lines
      let selections = case parseSelections (Text.strip selectionsString) of
            Nothing -> error "Couldn't parse string"
            Just m -> m
      return $ SelectCmd selections (parseModularProgram (Text.unlines rest))

-- readCommand :: IO (Maybe Text) -> IO (Maybe Command)
-- readCommand readText = do
--   cmd <- readText
--   case Text.strip <$> cmd of
--     Just "model-graph" -> Just . ModelGraphCmd . parseModularProgram <$> unfoldRead readText
--     Just "module-graph" -> Just . ModelGraphCmd . parseModularProgram <$> unfoldRead readText
--     Just "select" -> do
--       (Just selectionsString) <- readText
--       let selections = case parseSelections (Text.strip selectionsString) of
--             Nothing -> error "Couldn't parse string"
--             Just m -> m
--       program <- unfoldRead readText
--       return . Just $ SelectCmd selections (parseModularProgram program)
--     Nothing -> return Nothing

basedir = "/var/www/html/"

go :: Command -> IO Text
go (SelectCmd selections prog) = return . Text.unlines . linesConcreteProgram $ selectModules prog selections
go (ModelGraphCmd prog) = do
  id <- ((`mod` 1000000) . toNanoSecs) <$> getTime Realtime

  let f = "graphs/temp_model_graph_" <> show id

  let modelGraph = modelTree prog
  let f' = f <> ".json"
  Text.writeFile f' (modelGraphAlchemy modelGraph)

  -- let modelGraph = modelTreeGraph prog
  -- f' <- publishGraph f modelGraph

  renameFile f' (basedir <> f')
  putStrLn $ "making file " <> (basedir <> f')
  return . Text.pack $ f'
go (ModuleGraphCmd prog) = do
  let moduleGraph = moduleTreeGraph prog
  id <- ((`mod` 1000000) . toNanoSecs) <$> getTime Realtime
  let f = "graphs/temp_module_graph_" <> show id
  f' <- publishGraph f moduleGraph
  renameFile f' (basedir <> f')
  putStrLn $ "making file " <> (basedir <> f')
  return . Text.pack $ f'

      -- mapM_ Text.putStrLn $ stanModel

-- data ModelGraph = ModelGraph (Set ModelNode) [(ModelNode, ModelNode, DeltaModule)] deriving (Eq, Ord, Show)
modelGraphAlchemy :: ModelGraph -> Text
modelGraphAlchemy (ModelGraph nodes edges) = toObj [
    ("edges", toArr (map edgeObj edges))
  , ("nodes", toArr (map nodeObj (Set.toList nodes)))
  ]
  where selToID = Text.replace "-" "__" . clean . quote . noWhite . commaLines
        noWhite = Text.filter (not . isSeparator)
        commaLines = Text.intercalate "_____" . sort . Text.lines
        quote t = "\"" <> t <> "\""
        clean = Text.replace ":" "___"
        edgeObj (ModelNode n1, ModelNode n2, DeltaModule sig i1 i2) = toObj [
            ("source", selToID $ n1)
          , ("target", selToID $ n2)
          , ("sig", quote $ sig)
          , ("i_source", quote $ i2)
          , ("i_target", quote $ i2)
          ]
        nodeObj (ModelNode n) = toObj [
            ("cluster", clean . quote $ "cluster?")
          , ("id", selToID $ n)
          , ("caption", quote . Text.replace "\n" "\\n" $ n)
          , ("node_type", clean . quote $ "type?")
          ]
        toObj pairs = "{" <> Text.intercalate ",\n" (map (\(a, b) -> "\"" <> a <> "\": " <> b) pairs) <> "}"
        toArr elems = "[" <> Text.intercalate ",\n" elems <> "]"
