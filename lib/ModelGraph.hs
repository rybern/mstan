{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Map                 (Map)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Fix                 (refold)
import           Data.MemoUgly
import qualified Data.Graph               as Graph (graphFromEdges, topSort)

import           Types
import           ModuleTree hiding (Node)

type I2H = Map ImplID [SigName]
i2h :: ModularProgram -> Map ImplID [SigName]
i2h = implSigs

type H2I = Map SigName [ImplID]
h2i :: ModularProgram -> Map SigName [ImplID]
h2i = Map.mapWithKey (map . ImplID) . sigImpls

class FromModules a where
  initial :: Set SigName -> a
  expandImpl :: SigName -> [(ImplID, Set SigName)] -> a -> [a]

applyHole :: FromModules a => I2H -> H2I -> SigName -> [a] -> [a]
applyHole i2h h2i h = concatMap (expandImpl h impls)
  where impls = map (\i -> (i, Set.fromList . fromJust $ Map.lookup i i2h))
                . fromJust $ Map.lookup h h2i

applyHoles :: FromModules a => I2H -> H2I -> [a]
applyHoles i2h h2i =
  foldl (flip (applyHole i2h h2i)) [initial initialHoles] sortedHoles
  where sortedHoles = topoSort $ Map.map (concatMap (\i -> fromJust $ Map.lookup i i2h)) h2i
        initialHoles = Set.fromList . fromJust $ Map.lookup Root i2h

data Node = Node { unexpanded :: Set SigName, expanded :: Set ImplID }
  deriving (Eq, Ord, Show)

instance FromModules Node where
  initial holes = Node holes Set.empty
  expandImpl h impls original@(Node holes sel) =
    if h `Set.member` holes then
      [ Node (Set.union holes' (Set.delete h holes)) (Set.insert impl sel)
      | (impl, holes') <- impls]
    else
      [original]

allSelections :: ModularProgram -> Set Selection
allSelections p = Set.fromList . map (toSel . expanded) $ applyHoles (i2h p) (h2i p)

data Edges = Edge { left :: Node, right :: Node, delta :: ModuleDelta }
           | Node' { node :: Node }
  deriving (Eq, Ord, Show)

instance FromModules Edges where
  initial holes = Node' (initial holes)
  expandImpl h impls p@(Edge left right delta) =
    case (expandImpl h impls left, expandImpl h impls right) of
      ([l], right') -> map (\r -> Edge l r delta) right'
      (left', [r]) -> map (\l -> Edge l r delta) left'
      (left', right') -> zipWith (\l r -> Edge l r delta) left' right'
  expandImpl h impls (Node' node) = map Node' expandedNodes ++
    [ Edge l r (ModuleDelta h (Just i1) (Just i2))
    | (((ImplID _ i1, _), l), ((ImplID _ i2, _), r)) <- pairs (zip impls expandedNodes)]
    where expandedNodes = expandImpl h impls node

modelGraph :: ModularProgram -> SelectionGraph
modelGraph p = SelectionGraph {
    nodes = NodeSet . Set.fromList $ flip mapMaybe nodeEdges $ \case
        Node' (Node _ expanded) -> Just (toSel expanded)
        _ -> Nothing
  , edges = Set.fromList $ flip mapMaybe nodeEdges $ \case
      Edge (Node _ expanded1) (Node _ expanded2) delta -> Just (toSel expanded1, toSel expanded2, delta)
      _ -> Nothing
  }
  where nodeEdges = applyHoles (i2h p) (h2i p)

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p sel = Set.unions . flip Map.mapWithKey sel $ \s i ->
  Set.unions . flip map (filter (/= ImplID s i) (fromJust $ Map.lookup s h2i')) $ \i' ->
    Set.fromList . map (toSel . expanded) $ applyHoles (i2h p) (Map.insert s [i'] h2i'')
  where h2i' = h2i p
        h2i'' = Map.mapWithKey (\h i -> [ImplID h i]) sel `Map.union` h2i'

firstSelection :: ModularProgram -> Selection
firstSelection p = toSel . expanded . head $ applyHoles (i2h p) h2i'
  where h2i' = Map.map (pure . head) (h2i p)

pairs :: [a] -> [(a, a)]
pairs (x:xs) = map (x,) xs ++ pairs xs
pairs _ = []

topoSort :: Ord a => Map a [a] -> [a]
topoSort m = map ((\(sigName, _, _) -> sigName) . fromVertex) $ Graph.topSort g
  where (g, fromVertex, _) = Graph.graphFromEdges . map (\(k, vs) -> (k, k, vs)) $ Map.toList m

toSel :: Set ImplID -> Selection
toSel = Map.fromList . mapMaybe (\case ImplID h i -> Just (h, i); Root -> Nothing) . Set.toList




-- These are the operations we need to define on a graph-like object to build it by recursively consuming the module graph
class Subgraph a where
  cartesianProduct :: a -> a -> a
  join :: SigName -> a -> a -> a
  append :: Selection -> a -> a
  one :: Selection -> a

-- Build a node's graph given its subtrees' graphs; for use in `refold` or `unfold`
joinSubgraphs :: Subgraph a => ModuleBranch a -> a
joinSubgraphs (SigBranch _ []) = error "Holes should have at least one implementation."
joinSubgraphs (SigBranch sig subgraphs) = foldl1' (join sig) subgraphs
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
    NodeSet . Set.map (uncurry Map.union) . Set.filter noSiblings $ Set.cartesianProduct n1 n2
  join _ n1 n2 = n1 <> n2
  append selection (NodeSet n) = NodeSet $ Set.map (selection <>) n
  one selection = NodeSet (Set.singleton selection)

noNodes :: NodeSet
noNodes = NodeSet Set.empty

allSelections'' :: ModularProgram -> Set Selection
allSelections'' = unNodeSet . buildGraph

----------
-- Selection graph: set of nodes (selections) and set of edges (pairs of selections labeled with the signature that differentiates them)
----------

graphFromModulesartesianProduct
  :: ((Selection, Selection) -> Bool)
  -> SelectionGraph -> SelectionGraph -> SelectionGraph
graphFromModulesartesianProduct noSiblings' (SelectionGraph n1 e1) (SelectionGraph n2 e2) = SelectionGraph {
    nodes = cartesianProduct n1 n2
  , edges = Set.map (\((a, b, d), s) -> let s' = s `Map.difference` a in (a<>s', b<>s', d))
            . Set.filter (\((a, _, _), s) -> noSiblings' (a, s))
            $ Set.cartesianProduct e1 (unNodeSet n2) <> Set.cartesianProduct e2 (unNodeSet n1)
  }

instance Subgraph SelectionGraph where
  cartesianProduct g1 g2 = graphFromModulesartesianProduct noSiblings g1 g2
  join _ (SelectionGraph n1 e1) (SelectionGraph n2 e2) = SelectionGraph {
      nodes = n1 <> n2
    , edges = e1 <> e2 <>
              setMapMaybe addDelta (Set.cartesianProduct (unNodeSet n1) (unNodeSet n2))
    }
    where addDelta (a, b) = (a, b,) <$> singleSiblingDelta a b
  append selection (SelectionGraph n e) = SelectionGraph {
      nodes = append selection n
    , edges = Set.map (\(a, b, d) -> (selection <> a, selection <> b, d)) e
    }
  one selection = SelectionGraph (one selection) Set.empty

singleSiblingDelta :: Selection -> Selection -> Maybe ModuleDelta
singleSiblingDelta a b =
  (\case
    [delta] -> Just delta
    _ -> Nothing)
  . filter (\(ModuleDelta _ i1 i2) -> i1 /= i2)
  . Map.elems
  $ Map.intersectionWithKey (\sig i1 i2 -> ModuleDelta sig (Just i1) (Just i2)) a b

noSiblingsWithParents :: Set SigName -> (Selection, Selection) -> Bool
noSiblingsWithParents parents (s1, s2) = flip all parents $ \h ->
  case (h `Map.lookup` s1, h `Map.lookup` s2) of
    (Nothing, _) -> True
    (_, Nothing) -> True
    (Just i1, Just i2) -> i1 == i2

oneSibling :: (Selection, Selection) -> Bool
oneSibling (s1, s2) = (1 ==) . length . Map.filter id $ Map.intersectionWith (==) s1 s2

noSiblings :: (Selection, Selection) -> Bool
noSiblings (s1, s2) = and $ Map.intersectionWith (==) s1 s2

setMapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f = Set.fromList . mapMaybe f . Set.toList

modelGraph' :: ModularProgram -> SelectionGraph
modelGraph' = buildGraph


----------
-- Selection graph with hole set
----------

data GraphWithHoles = GraphWithHoles {
    graph :: SelectionGraph
  , visited :: Set SigName
  } deriving (Show, Eq, Ord)

instance Subgraph GraphWithHoles where
  cartesianProduct (GraphWithHoles g1 hs1) (GraphWithHoles g2 hs2) =
    GraphWithHoles {
      graph = graphFromModulesartesianProduct (noSiblingsWithParents (Set.intersection hs1 hs2)) g1 g2
    , visited = hs1 <> hs2
    }
  join h (GraphWithHoles (SelectionGraph n1 e1) hs1) (GraphWithHoles (SelectionGraph n2 e2) hs2) =
    GraphWithHoles {
      graph = SelectionGraph {
            nodes = n1 <> n2
          , edges = e1 <> e2 <> joinEdges
          }
    , visited = Set.insert h $ hs1 <> hs2
    }
    where delta = ModuleDelta h Nothing Nothing
          commonParents = Set.intersection hs1 hs2
          joinEdges
            = Set.map (\(s1, s2) -> (s1, s2, delta))
            . Set.filter (noSiblingsWithParents commonParents)
            $ Set.cartesianProduct (unNodeSet n1) (unNodeSet n2)
  append selection (GraphWithHoles g hs) = GraphWithHoles {
      graph = append selection g
    , visited = hs
    }
  one selection = GraphWithHoles (one selection) Set.empty

modelGraph'' :: ModularProgram -> SelectionGraph
modelGraph'' = graph . buildGraph

----------
-- Build up the "first" selection
----------

firstSelection' :: ModularProgram -> Selection
firstSelection' p = refold joinFirstSelection (growTree p) (Impl Root)
  where joinFirstSelection (SigBranch _ implSels) = head implSels
        joinFirstSelection (ImplBranch sel sigSels) = sel <> mconcat sigSels

----------
-- Efficiently build the neighbors of a selection
----------

-- Keep track of nodes with no differences and those with one difference
  -- (a "difference" is a sibling of an implementation in the selection)
data NeighborFromModulesandidates = NeighborFromModulesandidates {
      noDiffs :: NodeSet
    , oneDiffs :: NodeSet
    }
  deriving (Eq, Ord, Show)

instance Subgraph NeighborFromModulesandidates where
  cartesianProduct (NeighborFromModulesandidates n1 o1) (NeighborFromModulesandidates n2 o2) =
    NeighborFromModulesandidates {
      noDiffs= n1 `cartesianProduct` n2
    , oneDiffs= (o1 `cartesianProduct` n2) <> (o2 `cartesianProduct` n1)
    }
  join _ (NeighborFromModulesandidates n1 o1) (NeighborFromModulesandidates n2 o2) =
    NeighborFromModulesandidates (n1 <> n2) (o1 <> o2)
  append sel (NeighborFromModulesandidates n o) = NeighborFromModulesandidates (append sel n) (append sel o)
  one selection = NeighborFromModulesandidates (one selection) noNodes

propagateNeighborFromModulesandidates :: Selection -> Selection -> NeighborFromModulesandidates -> NeighborFromModulesandidates
propagateNeighborFromModulesandidates path sel (NeighborFromModulesandidates childrenNoDiffs childrenOneDiffs)
  | implInPath = NeighborFromModulesandidates { noDiffs= childrenNoDiffs, oneDiffs= childrenOneDiffs }
  | sigInPath  = NeighborFromModulesandidates { noDiffs= noNodes,         oneDiffs= childrenNoDiffs  }
  | otherwise  = NeighborFromModulesandidates { noDiffs= childrenNoDiffs, oneDiffs= noNodes          }
  where inter = Map.intersection path sel
        implInPath = Map.null sel || inter == sel
        sigInPath = not $ Map.null inter

modelNeighbors' :: ModularProgram -> Selection -> Set Selection
modelNeighbors' p path =
  unNodeSet . oneDiffs $ buildGraph' (propagateNeighborFromModulesandidates path) p

modelNeighbors'' :: ModularProgram -> Selection -> Set Selection
modelNeighbors'' p path =
  Set.filter (isJust . singleSiblingDelta path) . unNodeSet $ buildGraph p
{-

NSuccesses:binomial,OvershootModel:fixed,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:binomial,OvershootModel:parametric,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:binomial,PAngleSuccess:angle_success,PSuccess:angle_only
NSuccesses:binomial,PSuccess:logistic
NSuccesses:proportional,OvershootModel:fixed,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:proportional,OvershootModel:parametric,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:proportional,PAngleSuccess:angle_success,PSuccess:angle_only
NSuccesses:proportional,PSuccess:logistic

NSuccesses:binomial,OvershootModel:fixed,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:binomial,OvershootModel:parametric,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:binomial,PAngleSuccess:angle_success,PSuccess:angle_only
NSuccesses:binomial,PSuccess:logistic
NSuccesses:proportional,OvershootModel:fixed,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:proportional,OvershootModel:parametric,PAngleSuccess:angle_success,PDistanceSuccess:distance_success,PSuccess:angle_and_distance
NSuccesses:proportional,PAngleSuccess:angle_success,PSuccess:angle_only
-}
