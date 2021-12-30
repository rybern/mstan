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
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text

import           Graphviz
import           Types


------
-- Data structure manipulation for graph/tree abstractions
------

-- Mapping from each signatures to all of its implementations
sigImpls :: ModularProgram -> Map SigName (Set ImplName)
sigImpls p = Map.fromList
        . map (\impls -> let sig = implSignature (head impls)
                         in  (sig, Set.fromList $ map (implName) impls))
        . groupBy (\x y -> implSignature x == implSignature y)
        . sortOn implSignature
        $ implementations p

-- Mapping from each implementation to all of its signatures
implSigs :: ModularProgram -> Map ImplID (Set SigName)
implSigs p = implSigs'
  where
    codeSigs = Set.map (\(SigName sig, _, _) -> SigName sig) . moduleInstances
    moduleSigs :: Foldable t => t (ModularCode) -> Set SigName
    moduleSigs = Set.unions . concatMap (\code -> [codeSigs code])
    implSigs = Map.fromList
            . map (\impl -> ((Just (implSignature impl), implName impl) , moduleSigs impl))
            $ implementations p
    implSigs' =
        Map.insert (Nothing, ImplName "root") (moduleSigs (topProgram p)) implSigs

data ModuleTree = SigTree [ModuleTree] | ImplTree ImplID [ModuleTree] deriving Show

sigTree :: Map ImplID (Set SigName) -> Map SigName (Set ImplName) -> SigName -> ModuleTree
sigTree iToS sToI sig = SigTree $
  map (implTree iToS sToI . (Just sig, )) . Set.toList . fromJust $ Map.lookup sig sToI

implTree :: Map ImplID (Set SigName) -> Map SigName (Set ImplName) -> ImplID -> ModuleTree
implTree iToS sToI impl = ImplTree impl $
  map (sigTree iToS sToI) . Set.toList . fromJust $ Map.lookup impl iToS

moduleTree :: ModularProgram -> ModuleTree
moduleTree p = implTree (implSigs p) (sigImpls p) (Nothing, ImplName "root")


------
-- Visualizations
------

moduleTreeGraphviz :: ModularProgram -> Graphviz
moduleTreeGraphviz p = moduleGraphToDot
    (ModuleGraph allImpls allSigs (implSigs p) (sigImpls p))
  where
    allSigs  = Set.map (\(_, SigName sig) -> SigName sig) (signatures p)
    allImpls = (Nothing, ImplName "root") : map
        (\impl -> (Just (implSignature impl), implName impl))
        (implementations p)

drawASCIITreeLevel :: Int -> a -> (a -> Text) -> IO ()
drawASCIITreeLevel level a showA =
    Text.putStrLn $ (Text.replicate level " ") <> showA a

drawASCIITree
    :: (Ord a, Ord b)
    => Int
    -> a
    -> Map a (Set b)
    -> Map b (Set a)
    -> (a -> Text)
    -> (b -> Text)
    -> IO ()
drawASCIITree level root next other showA showB = do
    drawASCIITreeLevel level root showA
    let nexts = case Map.lookup root next of
            Just nexts -> nexts
            Nothing    -> Set.empty
    mapM_ (\n -> drawASCIITree (level + 2) n other next showB showA) nexts

drawModuleTree :: ModularProgram -> IO ()
drawModuleTree p = drawASCIITree 0
                            (Nothing, ImplName "root")
                            (implSigs p)
                            (sigImplsAddImplIDs $ sigImpls p)
                            (\(_, ImplName a) -> "[" <> a <> "]")
                            (\(SigName a) -> "{" <> a <> "}")
  where sigImplsAddImplIDs = Map.mapWithKey (\sig impls -> Set.map (Just sig,) impls)
