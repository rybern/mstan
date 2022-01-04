{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ModelGraph (
    modelGraph
  , allSelections
  , modelNeighbors
  , decoratedModelGraph
  , firstSelection
  , modelGraphviz
  ) where

import           Algebra.Graph
import           Algebra.Graph.Undirected (neighbours, toUndirected)
import           Data.List                (foldl1')
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Fix                 (refold)
import           Data.MemoUgly

import           Graphviz
import           Types
import           ModuleTree

-- Build a node's graph given its subtrees' graphs; for use in `refold` or `unfold`
-- Using algebraic graph construction, see https://hackage.haskell.org/package/algebraic-graphs-0.6
joinGraphs :: ModuleBranch (Graph Selection) -> Graph Selection
joinGraphs (SigBranch _ implGraphs) = foldl1' graphJoin implGraphs
  -- Signature nodes combine children by graph join:
  --   https://en.wikipedia.org/wiki/Graph_operations
  where graphJoin = connect
joinGraphs (ImplBranch impl []) = Vertex impl
joinGraphs (ImplBranch impl sigGraphs) = (impl <>) <$> foldl1' cartesianProduct sigGraphs
  -- Implementation nodes combine children by Cartesian graph product:
  --   https://en.wikipedia.org/wiki/Cartesian_product_of_graphs
  where cartesianProduct g1 g2 = uncurry (<>) <$> box g1 g2

-- Grow a module tree from Root with `growTree`, fold into a graph with `joinGraphs` (hylomorphism)
-- Use memoization because modules may not actually form a tree, so sub"trees" may be repeated
modelGraph :: ModularProgram -> Graph Selection
modelGraph p = refold (memo joinGraphs) (growTree p) (Impl Root)

allSelections :: ModularProgram -> Set Selection
allSelections = vertexSet . modelGraph

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p s = neighbours s . toUndirected $ modelGraph p

-- Faster than `Set.head . allSelections`
firstSelection :: ModularProgram -> Selection
firstSelection p = refold joinFirstSelection (growTree p) (Impl Root)
  where joinFirstSelection (SigBranch _ implSels) = head implSels
        joinFirstSelection (ImplBranch sel sigSels) = sel <> mconcat sigSels

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
