{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
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

selectionDelta :: Selection -> Selection -> Maybe ModuleDelta
selectionDelta a b =
  (\case
    [delta] -> Just delta
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
  where selectionDelta' (a, b) = (a, b,) <$> selectionDelta a b
        join (n1, e1) (n2, e2) =
          (n1 <> n2,
           e1 <> e2 <> setMapMaybe selectionDelta' (Set.cartesianProduct n1 n2))
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

firstSelection :: ModularProgram -> Selection
firstSelection p = refold joinFirstSelection (growTree p) (Impl Root)
  where joinFirstSelection (SigBranch _ implSels) = head implSels
        joinFirstSelection (ImplBranch sel sigSels) = sel <> mconcat sigSels

------
-- Efficient neighbors
------

-- When on the path, track the subpath like a node and neighbors like edges
-- When off the path, track nodes like normal except ignoring branches on path
data FoundNeighbors =
    -- Subpaths below this node that share no signatures with the model
    NoSharedSig (Set Selection)
  | Found {
      -- Neighbors of subpath
      subneighbors :: Set Selection
      -- Subpath of the model below this node
    , subpath :: Selection
    }
  deriving (Eq, Ord, Show)

-- Analogous to graph join with one node
joinAtSig :: FoundNeighbors -> FoundNeighbors -> FoundNeighbors
joinAtSig (NoSharedSig s1) (NoSharedSig s2) = NoSharedSig (s1 <> s2)
joinAtSig (Found f1 p1) (Found f2 p2) = Found (f1 <> f2) (max p1 p2)
joinAtSig a b = error $ "Mixed types at sig " ++ show a ++ ", " ++ show b

-- Analogous to graph cartesian product with one node
joinAtImpl :: FoundNeighbors -> FoundNeighbors -> FoundNeighbors
joinAtImpl (NoSharedSig s1) (NoSharedSig s2) =
  NoSharedSig . Set.map (uncurry (<>)) $ Set.cartesianProduct s1 s2
joinAtImpl (Found f1 p1) (Found f2 p2) =
  Found (Set.map (<> p2) f1 <> Set.map (<> p1) f2) (p1 <> p2)
joinAtImpl (NoSharedSig s) (Found _ p) = NoSharedSig (Set.map (<> p) s)
joinAtImpl (Found _ p) (NoSharedSig s) = NoSharedSig (Set.map (<> p) s)

joinNeighbors :: Selection -> ModuleBranch FoundNeighbors -> FoundNeighbors
joinNeighbors _ (SigBranch _ implStates) = if null implStates then (Found Set.empty Map.empty) else foldl1 joinAtSig  implStates
joinNeighbors path (ImplBranch sel sigStates) =
  let concatStates = foldl joinAtImpl (Found Set.empty Map.empty) sigStates
      inter = Map.intersectionWith (\a _ -> a) path sel in
    if Map.null sel || inter == sel then
      -- On path
      let (Found diffs subpath) = concatStates in
        Found (Set.map (sel <>) diffs) (sel <> subpath)
    else
      if Map.null inter then
        -- This is a non-neighboring impl, collect
        let (NoSharedSig s) = NoSharedSig (Set.singleton Map.empty) `joinAtImpl` concatStates in
          NoSharedSig (Set.map (sel <>) s)
      else
        -- Diff root
        case concatStates of
          NoSharedSig s -> Found (Set.map (sel <>) s) Map.empty
          Found _ subpath -> Found (Set.singleton (sel <> subpath)) subpath

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p path =
  let (Found neighbors _) = refold (memo (joinNeighbors path)) (growTree p) (Impl Root) in neighbors

modelNeighbors' :: ModularProgram -> Selection -> Set Selection
modelNeighbors' p path = Set.filter (isJust . selectionDelta path) . fst $ modelGraph p



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

{-

-}
