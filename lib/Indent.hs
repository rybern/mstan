{-# LANGUAGE OverloadedStrings #-}
module Indent where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Maybe

indentation :: Int -> Text
indentation n = Text.replicate n "  "

starIndentation :: Int -> Text
starIndentation 0 = ""
starIndentation 1 = "* "
starIndentation n = indentation (n - 1) <> starIndentation 1

starIndent :: Int -> [Text] -> [Text]
starIndent _ [] = []
starIndent n (l:ls) = indentNestedLines n $ (starIndentation n <> l) : (map (indentation n <>) ls)

indent :: Int -> [Text] -> [Text]
indent n = indentNestedLines n . map (indentation n <>)

-- Remove n leading spaces if all of the code has at least that many leading spaces
indentNestedLines :: Int -> [Text] -> [Text]
indentNestedLines n codeText = map indentCodeStmt codeText
  where indentCodeStmt = Text.intercalate "\n" . indentLines n . Text.lines
        indentLines _ [] = []
        indentLines n' (l:ls) = l:(map (indent' <>) ls)
          where indent' = indentation n'

-- Remove n leading spaces if all of the code has at least that many leading spaces
unindentNestedLines :: Int -> [Text] -> [Text]
unindentNestedLines n codeText = fromMaybe codeText $ mapM unindentCodeStmt codeText
  where unindentCodeStmt = ((Text.intercalate "\n" <$>) . unindentLines n . Text.lines)
        unindentLines :: Int -> [Text] -> Maybe [Text]
        unindentLines _ [] = Nothing
        unindentLines n' (l:ls) = (l:) <$> mapM (Text.stripPrefix indent') ls
          where indent' = indentation n'


whenNonempty :: [a] -> Maybe [a]
whenNonempty xs = if null xs then Nothing else Just xs
