{-# LANGUAGE LambdaCase #-}
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

import           Data.List                (foldl')
import           Data.Maybe
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

-- TODO:
  -- At sigs, custom graph join necessary. Union graphs but manually add edges between
  -- At impls, nodes can duplicate. Join identical nodes.

type SelectionGraph = (Set Selection, Set (Selection, Selection, ModuleDelta))

selectionDelta :: (Selection, Selection) -> Maybe (Selection, Selection, ModuleDelta)
selectionDelta (a, b) =
  (\case
    [delta] -> Just (a, b, delta)
    _ -> Nothing)
  . filter (\(ModuleDelta _ i1 i2) -> i1 /= i2)
  . Map.elems
  $ Map.intersectionWithKey (\(SigName sig) (ImplName impl1) (ImplName impl2) ->
                                    ModuleDelta sig impl1 impl2) a b

setMapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f = Set.fromList . mapMaybe f . Set.toList

-- Build a node's graph given its subtrees' graphs; for use in `refold` or `unfold`
joinGraphs :: ModuleBranch SelectionGraph -> SelectionGraph
joinGraphs (SigBranch _ implGraphs) = foldl' join (Set.empty, Set.empty) implGraphs
  -- Signature nodes combine children by graph join:
  --   https://en.wikipedia.org/wiki/Graph_operations
  where join (n1, e1) (n2, e2) =
          (n1 <> n2,
           e1 <> e2 <> setMapMaybe selectionDelta (Set.cartesianProduct n1 n2))
joinGraphs (ImplBranch impl sigGraphs) = foldl' product (Set.singleton impl, Set.empty) sigGraphs
  -- Implementation nodes combine children by Cartesian graph product:
  --   https://en.wikipedia.org/wiki/Cartesian_product_of_graphs
  where product (n1, e1) (n2, e2) =
          ( Set.map (uncurry (<>)) (Set.cartesianProduct n1 n2)
          , Set.map (\((a, b, d), s) -> (a<>s, b<>s, d))
            (Set.cartesianProduct e1 n2 <> Set.cartesianProduct e2 n1)
          )

-- Grow a module tree from Root with `growTree`, fold into a graph with `joinGraphs` (hylomorphism)
-- Use memoization because modules may not actually form a tree, so sub"trees" may be repeated
modelGraph :: ModularProgram -> SelectionGraph
modelGraph p = refold (memo joinGraphs) (growTree p) (Impl Root)

allSelections :: ModularProgram -> Set Selection
allSelections = fst . modelGraph

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p s = undefined

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
    (nodes, edges) = modelGraph p
    toNode = ModelNode . showSel
    allModels = Set.map toNode nodes
    modelEdges = map (\(a, b, d) -> (toNode a, toNode b, d)) . Set.toList $ edges

showSel :: Selection -> Text
showSel = Text.unlines
          . map (\(SigName sig, ImplName impl) -> sig <> ": " <> impl)
          . Map.toList
