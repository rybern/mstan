module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set
import Control.Monad

import CLI
import GraphServer

main :: IO ()
main = do
  serverOptions <- parseOptions
  runGraphServer serverOptions
