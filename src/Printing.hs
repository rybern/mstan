{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Printing where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Maybe
import qualified Data.Set                      as Set

import Types


printConcreteProgram :: ConcreteProgram -> IO ()
printConcreteProgram = mapM_ Text.putStrLn . linesConcreteProgram

indentation :: Int -> Text
indentation n = Text.replicate n "  "

indent :: Int -> [Text] -> [Text]
indent n = map (indentation n <>)

-- Remove n leading spaces if all of the code has at least that many leading spaces
unindentNestedCode :: Int -> [Text] -> [Text]
unindentNestedCode n codeText = fromMaybe codeText $ mapM unindentCodeStmt codeText
  where unindentCodeStmt = ((Text.intercalate "\n" <$>) . unindentLines n . Text.lines)
        unindentLines :: Int -> [Text] -> Maybe [Text]
        unindentLines n (l:ls) = (l:) <$> mapM (Text.stripPrefix indent) ls
          where indent = indentation n

linesCode :: ConcreteCode -> [Text]
linesCode = codeText . unconcreteCode

linesBlock :: Text -> [Text] -> [Text]
linesBlock name lines = concat [
    [name <> " {"]
  , indent 1 lines
  , [ "}"]
  ]

whenNonempty :: [a] -> Maybe [a]
whenNonempty xs = if null xs then Nothing else Just xs

lineParam :: Param -> Text
lineParam (Param p) = p <> ";"

linesConcreteProgram :: ConcreteProgram -> [Text]
linesConcreteProgram (ConcreteProgram {..}) = concat $ catMaybes
  [ (linesBlock "functions" <$>) . whenNonempty . unindentNestedCode 1 . linesCode $ concreteFunctions
  , Just . linesBlock "data" $ concreteData
  , (linesBlock "transformed data" <$>) . whenNonempty . unindentNestedCode 1 . linesCode $ concreteTD
  , Just . linesBlock "parameters" . map lineParam . Set.toList $ concreteParams
  , Just . linesBlock "model {" . linesCode $ concreteBody
  , (linesBlock "generated quantities" <$>) . whenNonempty . unindentNestedCode 1 . linesCode $ concreteGQ
  ]
