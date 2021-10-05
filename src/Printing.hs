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
unindentCodeText :: Int -> [Text] -> [Text]
unindentCodeText n codeText = fromMaybe codeText $ mapM unindentCodeStmt codeText
  where unindentCodeStmt = ((Text.intercalate "\n" <$>) . unindentLines n . Text.lines)
        unindentLines :: Int -> [Text] -> Maybe [Text]
        unindentLines n (l:ls) = (l:) <$> mapM (Text.stripPrefix indent) ls
          where indent = indentation n

-- data ConcreteProgram = ConcreteProgram
--   { concreteBody :: ConcreteCode,
--     concreteFunctions :: ConcreteCode,
--     concreteData :: [Text],
--     concreteParams :: Set Param,
--     concreteTD :: ConcreteCode,
--     concreteGQ :: ConcreteCode
--   } deriving Show

linesCode :: ConcreteCode -> [Text]
linesCode = indent 1 . unindentCodeText 1 . codeText . unconcreteCode

linesBlock :: Text -> [Text] -> [Text]
linesBlock name lines = concat [
    [name <> " {"]
  , lines
  , [ "}"]
  ]

whenNonempty :: [a] -> Maybe [a]
whenNonempty xs = if null xs then Nothing else Just xs

lineParam :: Param -> Text
lineParam (Param p) = p <> ";"

linesConcreteProgram :: ConcreteProgram -> [Text]
linesConcreteProgram (ConcreteProgram {..}) = concat $ catMaybes
  [ (linesBlock "functions" <$>) . whenNonempty . linesCode $ concreteFunctions
  , Just . linesBlock "data" . indent 1 $ concreteData
  , (linesBlock "transformed data" <$>) . whenNonempty . linesCode $ concreteTD
  , Just . linesBlock "parameters" . indent 1 . map lineParam . Set.toList $ concreteParams
  , Just . linesBlock "model {" . linesCode $ concreteBody
  , (linesBlock "generated quantities" <$>) . whenNonempty . linesCode $ concreteGQ
  ]

-- linesConcreteProgram :: ConcreteProgram -> [Text]
-- linesConcreteProgram p = concat
--   [ [ "functions {"]
--   , indent 1 (unindentCodeText 1 (codeText (unconcreteCode (concreteFunctions p))))
--   , [ "}"]
--   , [ "data {"]
--   , indent 1 (concreteData p)
--   , [ "}"]
--   , ["transformed data {"]
--   , indent 1 (unindentCodeText 1 (codeText (unconcreteCode (concreteTD p))))
--   , [ "}" ]
--   , ["parameters {"]
--   , map (\(Param p) -> "  " <> p <> ";") (Set.toList (concreteParams p))
--   , [ "}" ]
--   , [ "model {"]
--   , indent 1 (codeText (unconcreteCode (concreteBody p)))
--   , [ "}" ]
--   , [ "generated quantities {"]
--   , indent 1 (unindentCodeText 1 (codeText (unconcreteCode (concreteGQ p))))
--   , [ "}" ]
--   ]
