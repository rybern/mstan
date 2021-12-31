{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Printing where

import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           Data.Maybe
import qualified Data.Set                      as Set

import Types
import Indent

printConcreteProgram :: Program ConcreteCode -> IO ()
printConcreteProgram = mapM_ Text.putStrLn . linesConcreteProgram

linesCode :: ConcreteCode -> [Text]
linesCode = codeText . unconcreteCode

linesBlock :: Text -> [Text] -> [Text]
linesBlock name lines' = concat [
    [name <> " {"]
  , indent 1 $ lines'
  , [ "}"]
  ]


lineParam :: Param -> Text
lineParam (Param p) = p <> ";"

linesConcreteProgram :: Program ConcreteCode -> [Text]
linesConcreteProgram (Program {..}) = concat $ catMaybes
  [ (linesBlock "functions" <$>) . whenNonempty =<< linesCode <$> functions
  , Just . linesBlock "data" $ progData
  , (linesBlock "transformed data" <$>) . whenNonempty =<< linesCode <$> td
  , (linesBlock "parameters" <$>) . whenNonempty . map lineParam . Set.toList $ params
  , (linesBlock "transformed parameters" <$>) . whenNonempty =<< linesCode <$> tp
  , Just . linesBlock "model" =<< linesCode <$> model
  , (linesBlock "generated quantities" <$>) . whenNonempty =<< linesCode <$> gq
  ]
  where (Blocks {..}) = progBlocks
