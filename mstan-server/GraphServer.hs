{-# LANGUAGE OverloadedStrings #-}
module GraphServer
    (GraphServerOptions (..)
    , GraphServerDirectoryOptions (..)
    , defaultGraphServerOptions
    , defaultDirOptions
    , WSServerOptions (..)
    , runGraphServer)
where

import           System.FilePath
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import           Data.Bifunctor

import           System.Random
import           Data.Char
import           Data.List                      ( sort )

import           Types
import           Printing
import           Parsing
import           ModelGraph
import           ModuleTree
import           ConcreteProgram
import           Graphviz
import           WebSocketServer

data GraphServerOptions = GraphServerOptions {
    wsOptions :: WSServerOptions
  , dirOptions :: GraphServerDirectoryOptions
  }
  deriving Show

data GraphServerDirectoryOptions = GraphServerDirectoryOptions {
    graphSubdirectory :: FilePath
  , webserverRootDirectory :: FilePath
  }
  deriving Show

defaultGraphServerOptions :: GraphServerOptions
defaultGraphServerOptions = GraphServerOptions
    { wsOptions          = defaultWSServerOptions
    , dirOptions         = defaultDirOptions
    }

defaultDirOptions :: GraphServerDirectoryOptions
defaultDirOptions = GraphServerDirectoryOptions
    { graphSubdirectory = "graphs"
    , webserverRootDirectory = "/var/www/html"
    }

runGraphServer :: GraphServerOptions -> IO ()
runGraphServer options = runWSServer (wsOptions options) $ \text -> do
    let commandOrError = readCommand text
    print commandOrError
    either (return . reportError) (runCommand (dirOptions options)) commandOrError

data Command = ModelGraphCmd ModularProgram
             | ModuleGraphCmd ModularProgram
             | SelectCmd (Map SigName ImplName) ModularProgram
             deriving Show

data CommandError = ProgramError ProgramError
                  | SelectionError String
                  | CommandParseError String
             deriving Show

readCommand :: Text -> Either CommandError Command
readCommand msg = case Text.lines msg of
    (cmd : body) ->
      case Text.strip cmd of
        "model-graph" ->
          bimap ProgramError ModelGraphCmd . parseModularProgram $ Text.unlines body
        "module-graph" ->
          bimap ProgramError ModuleGraphCmd . parseModularProgram $ Text.unlines body
        "select" -> case body of
                      (selectionsString : rest) ->
                        case parseSelections (Text.strip selectionsString) of
                          Nothing -> Left . SelectionError $ "Couldn't parse string"
                          Just selections -> bimap ProgramError (SelectCmd selections) (parseModularProgram (Text.unlines rest))
                      _ -> Left . CommandParseError . Text.unpack $
                             "Failed to parse command " <> msg
        _ -> error . Text.unpack $ "Failed to parse command " <> msg
    _ -> error . Text.unpack $ "Failed to parse command " <> msg

generateID :: IO String
generateID = show . abs <$> (randomIO :: IO Int)

reportError :: CommandError -> Text
reportError (ProgramError programError) = "program-error\n" <>
  (Text.pack . unlines . showProgramError $ programError)
reportError (SelectionError e) = "selection-error\n" <> Text.pack e
reportError (CommandParseError e) = "command-error\n" <> Text.pack e

runCommand :: GraphServerDirectoryOptions -> Command -> IO Text
runCommand _ (SelectCmd selections prog) =
    return . ("concrete-program\n" <>) . Text.intercalate "\n" . linesConcreteProgram $ selectModules prog selections
runCommand dirs (ModelGraphCmd prog) = (("model-graph-filepath\n" <>) <$>) $
    publishGraphFile dirs "temp_model_graph" "json" $ \filePath -> do
      let graph = decoratedModelGraph (modelGraph prog)
      Text.writeFile filePath (modelGraphAlchemy graph)
runCommand dirs (ModuleGraphCmd prog) = (("module-graph-filepath\n" <>) <$>) $
    publishGraphFile dirs "temp_module_graph" "svg" $ \filePath -> do
      let moduleGraph = moduleTreeGraphviz prog
      publishGraph filePath moduleGraph

publishGraphFile :: GraphServerDirectoryOptions -> String -> String -> (FilePath -> IO ()) -> IO Text
publishGraphFile dirs fileTemplate fileExtension write = do
  fileSubpath <- (\fileID -> graphSubdirectory dirs </> fileTemplate <> "_" <> fileID <.> fileExtension) <$> generateID
  let filePath = webserverRootDirectory dirs </> fileSubpath

  putStr $ "Writing file " <> show filePath <> ".."
  write filePath
  putStrLn $ " ..done."
  return . Text.pack $ fileSubpath

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
    edgeObj (ModelNode n1, ModelNode n2, ModuleDelta (SigName sig) _ _) = toObj
        [ ("source"  , selToID $ n1)
        , ("target"  , selToID $ n2)
        , ("sig"     , quote $ sig)
        -- , ("i_source", quote $ i1)
        -- , ("i_target", quote $ i2)
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
