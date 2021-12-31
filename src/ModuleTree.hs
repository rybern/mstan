{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ModuleTree where



import           Data.List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text.IO             as Text
import Data.Fix

import           Graphviz
import           Types
import           Indent


------
-- Data structure manipulation for graph/tree abstractions
------

-- Mapping from each signature to all of its implementations
sigImpls :: ModularProgram -> Map SigName (Set ImplName)
sigImpls p = Map.fromList
        . map (\impls -> let sig = implSignature (head impls)
                         in  (sig, Set.fromList $ map implName impls))
        . groupBy (\x y -> implSignature x == implSignature y)
        . sortOn implSignature
        $ implementations p

-- Mapping from each implementation to all of its signatures
implSigs :: ModularProgram -> Map ImplID (Set SigName)
implSigs p = Map.insert Root (moduleSigs (topProgram p))
             . Map.fromList
             . map (\impl -> (ImplID (implSignature impl) (implName impl), moduleSigs impl))
            $ implementations p
  where
    codeSigs = Set.map (\(SigName sig, _, _) -> SigName sig) . moduleInstances
    moduleSigs :: Foldable t => t ModularCode -> Set SigName
    moduleSigs = Set.unions . concatMap (\code -> [codeSigs code])

-- Pattern functor for module tree
data ModuleTree f = SigTree SigName [f] | ImplTree Selection [f]
  deriving Functor

data Node = Impl ImplID | Sig SigName

-- Produce one level of tree given a node; for use in `hylo` or `ana`
growTree :: ModularProgram -> Node -> ModuleTree Node
growTree p = growTree' (implSigs p) (sigImpls p)

growTree' :: Map ImplID (Set SigName) -> Map SigName (Set ImplName) -> Node -> ModuleTree Node
growTree' iToS _ (Impl impl) =
  ImplTree (idToSel impl) . map Sig . Set.toList . fromJust $ Map.lookup impl iToS
growTree' _ sToI (Sig sig) =
  SigTree sig . map (Impl . ImplID sig) . Set.toList . fromJust $ Map.lookup sig sToI

idToSel :: ImplID -> Selection
idToSel Root              = Map.empty
idToSel (ImplID sig impl) = Map.singleton sig impl

------
-- Visualizations
------

-- Combine a node's subtrees' text representations
joinTextLines :: ModuleTree [Text] -> [Text]
joinTextLines (SigTree (SigName sig) implLines) =
  "[" <> sig <> "]" : concatMap (indent 1) implLines
joinTextLines (ImplTree sel sigLines) =
  selLine ++ concatMap (indent 1) sigLines
  where selLine = case Map.elems sel of
          [] -> ["(root)"]
          impls -> map (\(ImplName impl) -> "(" <> impl <> ")") impls

-- Build up a modular tree, fold it down to text lines, print it
printModularTree :: ModularProgram -> IO ()
printModularTree p = mapM_ Text.putStrLn $ hylo joinTextLines (growTree p) (Impl Root)

-- Representation of a modular tree in Graphviz format for output
moduleTreeGraphviz :: ModularProgram -> Graphviz
moduleTreeGraphviz p = moduleGraphToDot
    (ModuleGraph allImpls allSigs (implSigs p) (sigImpls p))
  where
    allSigs  = Set.map (\(_, SigName sig) -> SigName sig) (signatures p)
    allImpls = Root : map
        (\impl -> ImplID (implSignature impl) (implName impl))
        (implementations p)
