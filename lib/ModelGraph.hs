{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module ModelGraph (
    modelGraph
  , allSelections
  , modelNeighbors
  , firstSelection
  ) where

import           Data.List                (foldl1')
import           Data.Maybe
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Fix                 (refold)
import           Data.MemoUgly

import           Types
import           ModuleTree

-- These are the operations we need to define on a graph-like object to build it by recursively consuming the module graph
class Subgraph a where
  cartesianProduct :: a -> a -> a
  join :: a -> a -> a
  append :: Selection -> a -> a
  one :: Selection -> a

-- Build a node's graph given its subtrees' graphs; for use in `refold` or `unfold`
joinSubgraphs :: Subgraph a => ModuleBranch a -> a
joinSubgraphs (SigBranch _ []) = error "Holes should have at least one implementation."
joinSubgraphs (SigBranch _ subgraphs) = foldl1' join subgraphs
joinSubgraphs (ImplBranch impl []) = one impl
joinSubgraphs (ImplBranch impl subraphs) = append impl $ foldl1' cartesianProduct subraphs

-- Grow a module tree from Root with `growTree`, fold into a graph with `joinGraphs` (hylomorphism)
-- Use memoization because modules may not actually form a tree, so sub"trees" may be repeated
consumeModuleGraph :: Ord a => (ModuleBranch a -> a) -> ModularProgram -> a
consumeModuleGraph consumeTree p = refold (memo consumeTree) (growTree p) (Impl Root)

buildGraph :: (Ord a, Subgraph a) => ModularProgram -> a
buildGraph = consumeModuleGraph joinSubgraphs

-- Do an extra post-processing step for each iteration
buildGraph' :: (Ord a, Subgraph a) => (Selection -> a -> a) -> ModularProgram -> a
buildGraph' transform = consumeModuleGraph joinSubgraphs'
  where joinSubgraphs' branch@(ImplBranch impl _)= transform impl $ joinSubgraphs branch
        joinSubgraphs' branch = joinSubgraphs branch

----------
-- Set of "nodes", AKA sets of all possible selections
----------

instance Subgraph NodeSet where
  cartesianProduct (NodeSet n1) (NodeSet n2) =
    NodeSet $ Set.map (uncurry (<>)) (Set.cartesianProduct n1 n2)
  join (NodeSet n1) (NodeSet n2) = NodeSet (n1 <> n2)
  append selection (NodeSet n) = NodeSet $ Set.map (selection <>) n
  one selection = NodeSet (Set.singleton selection)

noNodes :: NodeSet
noNodes = NodeSet Set.empty

allSelections :: ModularProgram -> Set Selection
allSelections = unNodeSet . buildGraph

----------
-- Selection graph: set of nodes (selections) and set of edges (pairs of selections labeled with the signature that differentiates them)
----------

instance Subgraph SelectionGraph where
  cartesianProduct (SelectionGraph n1 e1) (SelectionGraph n2 e2) = SelectionGraph {
      nodes = cartesianProduct n1 n2
    , edges = Set.map (\((a, b, d), s) -> (a<>s, b<>s, d))
              (Set.cartesianProduct e1 (unNodeSet n2) <> Set.cartesianProduct e2 (unNodeSet n1))
    }
  join (SelectionGraph n1 e1) (SelectionGraph n2 e2) = SelectionGraph {
      nodes = join n1 n2
    , edges = e1 <> e2 <>
              setMapMaybe addSelectionDelta (Set.cartesianProduct (unNodeSet n1) (unNodeSet n2))
    }
    where addSelectionDelta (a, b) = (a, b,) <$> selectionDelta a b
  append selection (SelectionGraph n e) = SelectionGraph {
      nodes = append selection n
    , edges = Set.map (\(a, b, d) -> (selection <> a, selection <> b, d)) e
    }
  one selection = SelectionGraph (one selection) Set.empty

selectionDelta :: Selection -> Selection -> Maybe ModuleDelta
selectionDelta a b =
  (\case
    [delta] -> Just delta
    _ -> Nothing)
  . filter (\(ModuleDelta _ i1 i2) -> i1 /= i2)
  . Map.elems
  $ Map.intersectionWithKey ModuleDelta a b

setMapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f = Set.fromList . mapMaybe f . Set.toList

modelGraph :: ModularProgram -> SelectionGraph
modelGraph = buildGraph

----------
-- Build up the "first" selection
----------

firstSelection :: ModularProgram -> Selection
firstSelection p = refold joinFirstSelection (growTree p) (Impl Root)
  where joinFirstSelection (SigBranch _ implSels) = head implSels
        joinFirstSelection (ImplBranch sel sigSels) = sel <> mconcat sigSels

----------
-- Efficiently build the neighbors of a selection
----------

-- Keep track of nodes with no differences and those with one difference
  -- (a "difference" is a sibling of an implementation in the selection)
data NeighborCandidates = NeighborCandidates {
      noDiffs :: NodeSet
    , oneDiffs :: NodeSet
    }
  deriving (Eq, Ord, Show)

instance Subgraph NeighborCandidates where
  cartesianProduct (NeighborCandidates n1 o1) (NeighborCandidates n2 o2) =
    NeighborCandidates {
      noDiffs= n1 `cartesianProduct` n2
    , oneDiffs= (o1 `cartesianProduct` n2) `join` (o2 `cartesianProduct` n1)
    }
  join (NeighborCandidates n1 o1) (NeighborCandidates n2 o2) =
    NeighborCandidates (n1 `join` n2) (o1 `join` o2)
  append sel (NeighborCandidates n o) = NeighborCandidates (append sel n) (append sel o)
  one selection = NeighborCandidates (one selection) noNodes

propagateNeighborCandidates :: Selection -> Selection -> NeighborCandidates -> NeighborCandidates
propagateNeighborCandidates path sel (NeighborCandidates childrenNoDiffs childrenOneDiffs)
  | implInPath = NeighborCandidates { noDiffs= childrenNoDiffs, oneDiffs= childrenOneDiffs }
  | sigInPath  = NeighborCandidates { noDiffs= noNodes,         oneDiffs= childrenNoDiffs  }
  | otherwise  = NeighborCandidates { noDiffs= childrenNoDiffs, oneDiffs= noNodes          }
  where inter = Map.intersection path sel
        implInPath = Map.null sel || inter == sel
        sigInPath = not $ Map.null inter

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p path =
  unNodeSet . oneDiffs $ buildGraph' (propagateNeighborCandidates path) p

modelNeighbors' :: ModularProgram -> Selection -> Set Selection
modelNeighbors' p path =
  Set.filter (isJust . selectionDelta path) . unNodeSet $ buildGraph p
