{-# LANGUAGE ApplicativeDo, RecordWildCards #-}
module CLI where

import           Options.Applicative
import           GraphServer
import           Types
import qualified Parsing
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

data RunOptions =
    Server GraphServerOptions
  | Exec MStanFile (Maybe FilePath) ExecCommand
  deriving Show

newtype MStanFile = MStanFile { unMStanFile :: FilePath }
  deriving Show

data ExecCommand =
    GetNeighbors Selection
  | GetConcrete Selection
  | GetMinimumSelection
  | GetModelGraph
  | GetModuleGraph
  deriving Show

parseOptions :: IO RunOptions
parseOptions = execParser
    (info (parserOptions <**> helper)
          (fullDesc <> progDesc "A suite of tools for modular Stan files")
    )

parserOptions :: Parser RunOptions
parserOptions = hsubparser
    (  command
            "server"
            (info (Server <$> parserServerOptions)
                  (progDesc "Run a websocket server for a web frontend")
            )
    <> command
           "exec"
           (info (Exec <$> parserMStanFile <*> parserOutputFile <*> parserExecCommand)
                 (progDesc "Execute model network command")
           )
    )

parseSelection :: String -> Selection
parseSelection s = case Parsing.parseSelections (Text.pack s) of
    Just m  -> m
    Nothing -> error $ "Failed to parse selection: " ++ s

parserSelection :: Parser Selection
parserSelection = parseSelection <$> strOption
    (long "model-id" <> short 's' <> metavar "SELECTION_STRING" <> help
        "Selection set of a model to serve as the model ID"
    )

parserMStanFile :: Parser MStanFile
parserMStanFile = MStanFile <$> strOption
    (long "modular-stan-file" <> short 'f' <> metavar "FILE" <> help
        "File path of the input modular Stan file"
    )

parserOutputFile :: Parser (Maybe FilePath)
parserOutputFile = (\s -> if null s then Nothing else Just s) <$> strOption
    (long "output-file" <> short 'o' <> metavar "FILE" <> value "" <> help "Output file path"
    )

parserExecCommand :: Parser ExecCommand
parserExecCommand = hsubparser
    (  command
          "get-neighbors"
          (info (GetNeighbors <$> parserSelection)
                (progDesc "Get model IDs of the neighbors of a model")
          )
    <> command
           "get-model"
           (info (GetConcrete <$> parserSelection)
                 (progDesc "Get the concrete Stan model given a model ID")
           )
    <> command
           "get-module-graph"
           (info (pure GetModuleGraph)
                 (progDesc "Generate a representation of the module graph of the modular Stan program.")
           )
    <> command
           "get-model-graph"
           (info (pure GetModelGraph)
                 (progDesc "Generate a representation of the model graph of the modular Stan program.")
           )
    <> command
           "get-first-model"
           (info (pure GetMinimumSelection)
                 (progDesc "Get an arbitrary model ID")
           )
    )

parserServerOptions :: Parser GraphServerOptions
parserServerOptions = do
    serverPort <- option
        auto
        (  long "web-socket-port"
        <> metavar "PORT"
        <> value (port $ wsOptions defaultGraphServerOptions)
        <> help "The port to use for web socket for serving the web frontend."
        )
    graphFileDirectory <- option
        auto
        (  long "graph-file-directory"
        <> metavar "DIR"
        <> value (graphFileDirectory defaultGraphServerOptions)
        <> help "The port to use for web socket for serving the web frontend."
        )
    return $ GraphServerOptions { wsOptions = WSServerOptions serverPort
                                , graphFileDirectory = graphFileDirectory
                                }
