{-# LANGUAGE OverloadedStrings #-}
module GraphServer
    (GraphServerOptions (..)
    , defaultGraphServerOptions
    , WSServerOptions (..)
    , runGraphServer)
where

import           System.Directory
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import           System.Random
import           Data.Char
import           Data.List

import           Types
import           Printing
import           Parsing
import           ModularStan
import           ToGraph

-- No longer using TCP server; switched to WebSockets
-- import WebServer
import           WebSocketServer

data GraphServerOptions = GraphServerOptions {
    wsOptions :: WSServerOptions
  , graphFileDirectory :: FilePath
  }
  deriving Show

defaultGraphServerOptions = GraphServerOptions
    { wsOptions          = defaultWSServerOptions
    , graphFileDirectory = "/var/www/html/"
    }

runGraphServer :: GraphServerOptions -> IO ()
runGraphServer options = runWSServer (wsOptions options) $ \text -> do
    readCommand text >>= \c ->
        (print c *> return c) >>= runCommand (graphFileDirectory options)

data Command = ModelGraphCmd ModularProgram
             | ModuleGraphCmd ModularProgram
             | SelectCmd (Map SigName ImplName) ModularProgram
             deriving Show


readCommand :: Text -> IO Command
readCommand msg = do
    let (cmd : lines) = Text.lines msg
    case Text.strip cmd of
        "model-graph" ->
            return . ModelGraphCmd . parseModularProgram $ Text.unlines lines
        "module-graph" ->
            return . ModuleGraphCmd . parseModularProgram $ Text.unlines lines
        "select" -> do
            let (selectionsString : rest) = lines
            let selections =
                    case parseSelections (Text.strip selectionsString) of
                        Nothing -> error "Couldn't parse string"
                        Just m  -> m
            return $ SelectCmd selections
                               (parseModularProgram (Text.unlines rest))

generateID :: IO String
generateID = (show . abs) <$> (randomIO :: IO Int)

runCommand :: FilePath -> Command -> IO Text
runCommand _ (SelectCmd selections prog) =
    return . Text.intercalate "\n" . linesConcreteProgram $ selectModules prog selections
runCommand fileDir (ModelGraphCmd prog) = do
    let graph = modelGraph prog

    fileName <-
        (\id -> "graphs/temp_model_graph_" <> id <> ".json") <$> generateID
    let filePath = fileDir <> fileName

    Text.writeFile fileName (modelGraphAlchemy graph)

    renameFile fileName filePath
    putStrLn $ "making file " <> filePath
    return . Text.pack $ fileName
runCommand fileDir (ModuleGraphCmd prog) = do
    let moduleGraph = moduleTreeGraphviz prog

    graphName <- ("graphs/temp_model_graph_" <>) <$> generateID
    fileName  <- publishGraph graphName moduleGraph
    let filePath = fileDir <> fileName

    renameFile fileName filePath
    putStrLn $ "making file " <> filePath
    return . Text.pack $ fileName

-- data ModelGraph = ModelGraph (Set ModelNode) [(ModelNode, ModelNode, DeltaModule)] deriving (Eq, Ord, Show)
modelGraphAlchemy :: ModelGraph -> Text
modelGraphAlchemy (ModelGraph nodes edges) = toObj
    [ ("edges", toArr (map edgeObj edges))
    , ("nodes", toArr (map nodeObj (Set.toList nodes)))
    ]
  where
    selToID    = Text.replace "-" "__" . clean . quote . noWhite . commaLines
    noWhite    = Text.filter (not . isSeparator)
    commaLines = Text.intercalate "_____" . sort . Text.lines
    quote t = "\"" <> t <> "\""
    clean = Text.replace ":" "___"
    edgeObj (ModelNode n1, ModelNode n2, DeltaModule sig i1 i2) = toObj
        [ ("source"  , selToID $ n1)
        , ("target"  , selToID $ n2)
        , ("sig"     , quote $ sig)
        , ("i_source", quote $ i1)
        , ("i_target", quote $ i2)
        ]
    nodeObj (ModelNode n) = toObj
        [ ("cluster"  , clean . quote $ "cluster?")
        , ("id"       , selToID $ n)
        , ("caption"  , quote . Text.replace "\n" "\\n" $ n)
        , ("node_type", clean . quote $ "type?")
        ]
    toObj pairs =
        "{"
            <> Text.intercalate
                   ",\n"
                   (map (\(a, b) -> "\"" <> a <> "\": " <> b) pairs)
            <> "}"
    toArr elems = "[" <> Text.intercalate ",\n" elems <> "]"
