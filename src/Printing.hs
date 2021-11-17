{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Printing where

import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           Data.Maybe
import qualified Data.Set                      as Set

import Types
import Indent

printConcreteProgram :: ConcreteProgram -> IO ()
printConcreteProgram = mapM_ Text.putStrLn . linesConcreteProgram

linesCode :: ConcreteCode -> [Text]
linesCode = codeText . unconcreteCode

linesBlock :: Text -> [Text] -> [Text]
linesBlock name lines = concat [
    [name <> " {"]
  , indent 1 $ lines
  , [ "}"]
  ]


lineParam :: Param -> Text
lineParam (Param p) = p <> ";"

linesConcreteProgram :: ConcreteProgram -> [Text]
linesConcreteProgram (ConcreteProgram {..}) = concat $ catMaybes
  [ (linesBlock "functions" <$>) . whenNonempty . linesCode $ concreteFunctions
  , Just . linesBlock "data" $ concreteData
  , (linesBlock "transformed data" <$>) . whenNonempty . linesCode $ concreteTD
  , (linesBlock "parameters" <$>) . whenNonempty . map lineParam . Set.toList $ concreteParams
  , (linesBlock "transformed parameters" <$>) . whenNonempty . linesCode $ concreteTP
  , Just . linesBlock "model" . linesCode $ concreteModel
  , (linesBlock "generated quantities" <$>) . whenNonempty . linesCode $ concreteGQ
  ]
