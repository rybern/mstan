{-# LANGUAGE ApplicativeDo, RecordWildCards #-}
module CLI where

import qualified Data.Text                     as Text
import           Options.Applicative           hiding (command)
import qualified Options.Applicative           as OptParse

import           Types
import qualified Parsing


data DebugParse = Silent | DebugParse deriving (Show, Eq)

data RunOptions = RunOptions {
    inFile :: MStanFile
  , debugParse :: DebugParse
  , outFile:: Maybe FilePath
  , command :: ExecCommand
  }
  deriving Show

data ExecCommand =
    GetNeighbors Selection
  | GetConcrete Selection
  | GetMinimumSelection
  | GetModelGraph
  | GetModuleGraph
  | GetAllModels
  deriving Show

parseOptions :: IO RunOptions
parseOptions = execParser
  (info (parserOptions <**> helper)
    (fullDesc <> progDesc "Execute model network command"))
  where parserOptions = RunOptions <$>
          parserMStanFile <*> parserDebugParse <*> parserOutputFile <*> parserExecCommand

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

parserExecCommand :: Parser ExecCommand
parserExecCommand = hsubparser
    (  OptParse.command
          "get-neighbors"
          (info (GetNeighbors <$> parserSelection)
                (progDesc "Get model IDs of the neighbors of a model")
          )
    <> OptParse.command
           "get-model"
           (info (GetConcrete <$> parserSelection)
                 (progDesc "Get the concrete Stan model given a model ID")
           )
    <> OptParse.command
           "get-module-graph"
           (info (pure GetModuleGraph)
                 (progDesc "Generate a representation of the module graph of the modular Stan program.")
           )
    <> OptParse.command
           "get-model-graph"
           (info (pure GetModelGraph)
                 (progDesc "Generate a representation of the model graph of the modular Stan program.")
           )
    <> OptParse.command
           "get-first-model"
           (info (pure GetMinimumSelection)
                 (progDesc "Get an arbitrary model ID")
           )
    <> OptParse.command
           "get-all-models"
           (info (pure GetAllModels)
                 (progDesc "Get all model IDs")
           )
    )
