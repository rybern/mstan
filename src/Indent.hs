{-# LANGUAGE OverloadedStrings #-}
module Indent where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Maybe

indentation :: Int -> Text
indentation n = Text.replicate n "  "

indent :: Int -> [Text] -> [Text]
indent n = map (indentation n <>)

-- Remove n leading spaces if all of the code has at least that many leading spaces
indentNestedLines :: Int -> [Text] -> [Text]
indentNestedLines n codeText = map indentCodeStmt codeText
  where indentCodeStmt = Text.intercalate "\n" . indentLines n . Text.lines
        indentLines n (l:ls) = l:(map (indent <>) ls)
          where indent = indentation n

-- Remove n leading spaces if all of the code has at least that many leading spaces
unindentNestedLines :: Int -> [Text] -> [Text]
unindentNestedLines n codeText = fromMaybe codeText $ mapM unindentCodeStmt codeText
  where unindentCodeStmt = ((Text.intercalate "\n" <$>) . unindentLines n . Text.lines)
        unindentLines :: Int -> [Text] -> Maybe [Text]
        unindentLines n (l:ls) = (l:) <$> mapM (Text.stripPrefix indent) ls
          where indent = indentation n


