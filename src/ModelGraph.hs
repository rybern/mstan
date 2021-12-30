{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ModelGraph (
    modelGraph
  , allSelections
  , arbitrarySelection
  , modelNeighbors
  , decoratedModelGraph
  , treeModelGraph
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

import           Graphviz
import           Types
import           ModuleTree

modelGraph :: ModularProgram -> Graph Selection
modelGraph = treeModelGraph . moduleTree

treeModelGraph :: ModuleTree -> Graph Selection
treeModelGraph (SigTree impls) = foldl1' connect $ map treeModelGraph impls
treeModelGraph (ImplTree name []) = Vertex (idToSel name)
treeModelGraph (ImplTree name sigs) =
  fmap (idToSel name <>) . foldl1' box' $ map treeModelGraph sigs
  where box' g1 g2 = (\(a, b) -> a <> b) <$> box g1 g2

idToSel :: ImplID -> Selection
idToSel (Nothing, _)     = Map.empty
idToSel (Just sig, impl) = Map.singleton sig impl

allSelections :: ModularProgram -> Set Selection
allSelections = vertexSet . modelGraph

arbitrarySelection :: ModularProgram -> Selection
arbitrarySelection = Set.findMin . allSelections

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p s = neighbours s . toUndirected $ modelGraph p

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
showSel =
    Text.unlines
        . map (\(SigName sig, ImplName impl) -> sig <> ": " <> impl)
        . Map.toList
