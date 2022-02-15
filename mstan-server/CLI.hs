{-# LANGUAGE ApplicativeDo, RecordWildCards #-}
module CLI where

import qualified Data.Text                     as Text
import           Options.Applicative

import           GraphServer
import           Types
import qualified Parsing


data DebugParse = Silent | DebugParse deriving (Show, Eq)

parseOptions :: IO GraphServerOptions
parseOptions = execParser
    (info (parserServerOptions <**> helper)
          (fullDesc <> progDesc "Run a websocket server for a web frontend")
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

parserDebugParse :: Parser DebugParse
parserDebugParse = flag Silent DebugParse
    (long "debug-parse" <> short 'v' <> help "Show parsed modular program data structure"
    )

parserOutputFile :: Parser (Maybe FilePath)
parserOutputFile = (\s -> if null s then Nothing else Just s) <$> strOption
    (long "output-file" <> short 'o' <> metavar "FILE" <> value "" <> help "Output file path"
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
    graphFileDirectory <- strOption
        (  long "graph-file-directory"
        <> metavar "DIR"
        <> value (graphFileDirectory defaultGraphServerOptions)
        <> help "The port to use for web socket for serving the web frontend."
        )
    return $ GraphServerOptions { wsOptions = WSServerOptions serverPort
                                , graphFileDirectory = graphFileDirectory
                                }
