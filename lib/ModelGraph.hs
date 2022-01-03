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

import           Algebra.Graph
import           Algebra.Graph.Undirected (neighbours, toUndirected)
import           Data.List                (foldl1')
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Fix                 (refold)
import           Data.Fix                 (foldFix, Fix(..))

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
modelGraph :: ModularProgram -> Graph Selection
modelGraph p = refold joinGraphs (growTree p) (Impl Root)

allSelections :: ModularProgram -> Set Selection
allSelections = vertexSet . modelGraph

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p s = neighbours s . toUndirected $ modelGraph p

-- Faster than `Set.head . allSelections`
firstSelection :: ModularProgram -> Selection
firstSelection p = refold joinFirstSelection (growTree p) (Impl Root)
  where joinFirstSelection (SigBranch _ implSels) = head implSels
        joinFirstSelection (ImplBranch sel sigSels) = sel <> mconcat sigSels

data NState =
    Virgins (Set Selection)
  | Found (Set Selection) Selection
  deriving Show

-- instance Semigroup NState where
--   (<>) (Virgins s1) (Virgins s2) = Virgins (s1 <> s2)
--   (<>) (Found f1 p1) (Found f2 p2) = Found (f1 <> f2) (max p1 p2)
--   (<>) (Virgins s) (Found _ p) = Virgins (Set.map (p <>) s)
--   (<>) (Found _ p) (Virgins s) = Virgins (Set.map (p <>) s)
-- instance Monoid NState where
--   mempty = Found Set.empty Map.empty

joinAtSig :: NState -> NState -> NState
joinAtSig (Virgins s1) (Virgins s2) = Virgins (s1 <> s2)
joinAtSig (Found f1 p1) (Found f2 p2) = Found (f1 <> f2) (max p1 p2)
joinAtSig a b = error $ "Mixed types at sig " ++ show a ++ ", " ++ show b

joinAtImpl :: NState -> NState -> NState
joinAtImpl (Virgins s1) (Virgins s2) = Virgins $ Set.fromList
  [sel1 <> sel2 | sel1 <- Set.toList s1, sel2 <- Set.toList s2]
joinAtImpl (Found f1 p1) (Found f2 p2) = Found (f1 <> f2) (max p1 p2)
joinAtImpl (Virgins s) (Found _ p) = Virgins (Set.map (p <>) s)
joinAtImpl (Found _ p) (Virgins s) = Virgins (Set.map (p <>) s)

joinNeighbors :: Selection -> ModuleBranch NState -> NState
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
        -- This is a virgin impl, collect
        let (Virgins s) = Virgins (Set.singleton Map.empty) `joinAtImpl` concatStates in
          Virgins (Set.map (sel <>) s)
      else
        -- Diff root
        case concatStates of
          Virgins s -> Found (Set.map (sel <>) s) Map.empty
          Found _ subpath -> Found (Set.singleton (sel <> subpath)) subpath

test3 = neighbors' tree path
  where tree =
          impl [] [
          sig 2 [
              impl [(2, "C")] []
              ]
          ]
        path = sel [(2, "C")]

test2 = neighbors' tree path
  where tree =
          impl [] [
          sig 2 [
              impl [(2, "C")] [],
              impl [(2, "D")] []
              ]
          ]
        path = sel [(2, "C")]

sel = Map.fromList . map (\(s, i) -> (SigName (Text.pack $ show s), ImplName i))
impl selection branches = Fix (ImplBranch (sel selection) branches)
sig s branches = Fix (SigBranch (SigName (Text.pack $ show s)) branches)
showSigs :: Set Selection -> IO ()
showSigs = mapM_ (print . showSelection)

test1Tree =
  impl [] [
    sig 2 [
        impl [(2, "C")] [
            sig 1 [
                impl [(1, "B")] [
                    sig 4 [
                        impl [(4, "F")] []
                        ]
                    ],
                impl [(1, "A")] [
                    sig 3 [
                        impl [(3, "E")] []
                        ]
                    ]
                ]
              ],
          impl [(2, "D")] [
            sig 1 [
                impl [(1, "B")] [
                    sig 4 [
                        impl [(4, "F")] []
                        ]
                    ],
                impl [(1, "A")] [
                    sig 3 [
                        impl [(3, "E")] []
                        ]
                    ]
                ]
              ]
          ]
      ]

test1Path = sel [(2, "C"), (1, "B"), (4, "F")]

test1 = neighbors' test1Tree test1Path


-- An issue with the fix approach: module tree is not strictly recursive. You could write a small module graph that produces a very inefficient "tree"

-- Grow a module tree from Root with `growTree`, fold into a graph with `joinGraphs` (hylomorphism)
neighbors' :: Fix ModuleBranch -> Selection -> Set Selection
neighbors' tree path =
  let (Found neighbors _) = foldFix (joinNeighbors path) tree in neighbors

-- Grow a module tree from Root with `growTree`, fold into a graph with `joinGraphs` (hylomorphism)
neighbors :: ModularProgram -> Selection -> Set Selection
neighbors p path =
  let (Found neighbors _) = refold (joinNeighbors path) (growTree p) (Impl Root) in neighbors

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
