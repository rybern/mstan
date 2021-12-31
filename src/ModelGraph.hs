{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ModelGraph (
    modelGraph
  , allSelections
  , arbitrarySelection
  , modelNeighbors
  , decoratedModelGraph
  , modelGraphviz
  ) where

import           Algebra.Graph
import           Algebra.Graph.Undirected (neighbours, toUndirected)
import           Data.List
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import Data.Fix

import           Graphviz
import           Types
import           ModuleTree

allSelections :: ModularProgram -> Set Selection
allSelections = vertexSet . modelGraph

arbitrarySelection :: ModularProgram -> Selection
arbitrarySelection = Set.findMin . allSelections

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p s = neighbours s . toUndirected $ modelGraph p

-- Build a node's graph given its subtrees' graphs; for use in `hylo` or `cata`
joinGraphs :: ModuleTree (Graph Selection) -> Graph Selection
joinGraphs (SigTree _ implGraphs) = foldl1' graphJoin implGraphs
  -- Signature nodes combine children with graph join:
  --   https://en.wikipedia.org/wiki/Graph_operations
  where graphJoin = connect
joinGraphs (ImplTree impl []) = Vertex impl
joinGraphs (ImplTree impl sigGraphs) = (impl <>) <$> foldl1' cartesianMAppend sigGraphs
  -- Implementation nodes combine children with graph Cartesian product:
  --   https://en.wikipedia.org/wiki/Cartesian_product_of_graphs
  where cartesianMAppend g1 g2 = (\(a, b) -> a <> b) <$> box g1 g2

-- Grow a module tree from Root with `growTree`, fold into a graph with `joinGraphs` (Hylomorphism)
modelGraph :: ModularProgram -> Graph Selection
modelGraph p = hylo joinGraphs (growTree p) (Impl Root)


------
-- Visualizations
------

modelGraphviz :: ModularProgram -> Graphviz
modelGraphviz p = modelGraphToDot (decoratedModelGraph p)

-- Node- and Edge-set representation of the graph of models for visualization
decoratedModelGraph :: ModularProgram -> ModelGraph
decoratedModelGraph p = ModelGraph allModels modelEdges
  where
    graph = modelGraph p
    toNode = ModelNode . showSel
    allModels = Set.map toNode (vertexSet graph)
    modelEdges = map edgeDelta . Set.toList $ edgeSet graph

    selectionDeltas :: Selection -> Selection -> [ModuleDelta]
    selectionDeltas a b =
        filter (\(ModuleDelta _ i1 i2) -> i1 /= i2)
      . Map.elems
      $ Map.intersectionWithKey (\(SigName sig) (ImplName impl1) (ImplName impl2) ->
                                    ModuleDelta sig impl1 impl2) a b

    edgeDelta :: (Selection, Selection) -> (ModelNode, ModelNode, ModuleDelta)
    edgeDelta (a, b) = case selectionDeltas a b of
      [delta] -> (toNode a, toNode b, delta)
      ds -> error $ "Internal issue: neighboring selections differ by !=1 key. " ++ show ds

showSel :: Selection -> Text
showSel = Text.unlines
          . map (\(SigName sig, ImplName impl) -> sig <> ": " <> impl)
          . Map.toList
