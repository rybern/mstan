{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Mockup where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import ToGraph

type Symbol = String

-- Stan-ish types
newtype Type = Type String deriving (Eq, Ord)

newtype Expr = Expr String deriving (Eq, Ord)

data Code = Code
  { moduleReferences :: Set (InstanceName, Reference),
    moduleInstances :: Set (SigName, Maybe InstanceName, [Expr]),
    codeString :: String,
    codeReturn :: Maybe Expr
  }
  deriving (Eq, Ord)

data Param = Param String deriving (Eq, Ord)

-- Module system types
type SigName = Symbol

type ImplName = Symbol

type InstanceName = Symbol

data Reference = Reference String deriving (Eq, Ord)

-- Program types

-- Constraint:
-- for all `implementations`, the body follows the matching signature in `signatures`
data ModularProgram = ModularProgram
  { signatures :: Set (Type, SigName), -- Constraints: Unique SigName
    implementations :: Set ModuleImplementation,
    topBody :: Code,
    topParams :: Set Param
  }

data ModuleImplementation = ModuleImplementation
  { implBody :: Code,
    implArgs :: [Symbol],
    implSignature :: SigName,
    implParams :: Set Param,
    implName :: ImplName
  }
  deriving (Eq, Ord)

data ConcreteProgram = ConcreteProgram
  { concreteBody :: Code,
    concreteParams :: Set Param
  }

-- Constraint:
-- The selection map is total for all signatures in the program
applyModuleSelections ::
  ModularProgram -> Map SigName ImplName -> ConcreteProgram
applyModuleSelections program selectionNames =
  ConcreteProgram
    { concreteBody =
        applyModuleReferences
          appliedSigImplementations
          (topBody program),
      concreteParams = Set.union (topParams program) moduleParams
    }
  where
    selections =
      Map.map
        ( \name ->
            fromJust
              ( find (\impl -> name == implName impl) (implementations program)
              )
        )
        selectionNames
    moduleParams = Set.unions (Map.map implParams selections)
    applyModules = applyModuleInitializers selections
    applyOrderSigs = topologicallyOrderSignatures selections
    appliedSigImplementations =
      foldl
        ( \impls sigName ->
            Map.update
              ( \impl ->
                  Just
                    ( impl
                        { implBody = applyModuleReferences impls (implBody impl)
                        }
                    )
              )
              sigName
              impls
        )
        selections
        applyOrderSigs

instanceInitializers ::
  Map SigName ModuleImplementation -> Code -> Map InstanceName Code
instanceInitializers sigImpls code = undefined inits
  where
    inits = moduleInstances code

applyModuleReferences :: Map SigName ModuleImplementation -> Code -> Code
applyModuleReferences sigImpls code =
  Set.fold
    applyReference
    code
    translatedReferences
  where
    instances = instanceInitializers sigImpls code
    references = moduleReferences code
    translatedReferences =
      Set.map
        ( \(instanceName, reference) ->
            (fromJust (Map.lookup instanceName instances), reference)
        )
        references

{-
0. Assign arguments
1. Insert the code beforehand with arguments
2. Replace with return expression or nothing
-}

applyModuleInitializers :: Map SigName ModuleImplementation -> Code -> Code
applyModuleInitializers sigImpls code = undefined

-- Set.fold applyInit
-- code
-- (moduleInstances code)
-- where modules =

applyInit ::
  Code ->
  (SigName, Maybe InstanceName, [Expr]) ->
  Map SigName ModuleImplementation ->
  Code
applyInit code (sigName, _, argnames) map = code
  where
    Just defn = Map.lookup sigName map

-- withArgs = (. map (\(argName, arg) -> "argName = arg;"). unlines $  ) ++ codeString defn

-- moduleReferences :: Code -> Set (InstanceName, Reference)
-- moduleReferences =
--   undefined

-- moduleInstances :: Code -> Set (SigName, InstanceName, [Expr])
-- moduleInstances = undefined

applyReference :: (Code, Reference) -> Code -> Code
applyReference = undefined

topologicallyOrderSignatures :: Map SigName ModuleImplementation -> [SigName]
topologicallyOrderSignatures = undefined

-- Nouns:

-- Module signature
type ModuleSignature = (Type, SigName)

-- Module instantiation
type ModuleInstantiation = (SigName, InstanceName, [Expr])

-- Module implementation
type ModuleImplementation' = ModuleImplementation

-- Modular program
type ModularProgram' = ModularProgram

-- Concrete program/model
type ConcreteProgram' = ConcreteProgram

-- Parameter set
type ParameterSet = Set Param

-- Module application
type ModuleApplication =
  ModularProgram -> Map SigName ImplName -> ConcreteProgram

-- Module reference
type ModuleReference = Reference

-- Module selection
type ModuleSelection = (SigName, ImplName)

data Sig = Sig String deriving (Show, Eq, Ord)

data Impl = Impl String deriving (Show, Eq, Ord)

drawTreeLevel :: Int -> a -> (a -> String) -> IO ()
drawTreeLevel level a showA = putStrLn $ (replicate level ' ') ++ showA a

drawTree ::
  (Ord a, Ord b) =>
  Int ->
  a ->
  Map a (Set b) ->
  Map b (Set a) ->
  (a -> String) ->
  (b -> String) ->
  IO ()
drawTree level root next other showA showB = do
  drawTreeLevel level root showA
  let nexts = case Map.lookup root next of
        Just nexts -> nexts
        Nothing -> Set.empty
  mapM_ (\n -> drawTree (level + 2) n other next showB showA) nexts

drawModuleTree :: ModularProgram -> IO ()
drawModuleTree p =
  drawTree
    0
    (Impl "root")
    sigs
    sigImpls
    (\(Impl a) -> "[" ++ a ++ "]")
    (\(Sig a) -> "{" ++ a ++ "}")
  where
    codeSigs = Set.map (\(sig, _, _) -> Sig sig) . moduleInstances
    implSigs =
      Map.fromList
        . map (\impl -> (Impl $ implName impl, codeSigs (implBody impl)))
        $ (Set.toList $ implementations p)
    sigs = Map.insert (Impl "root") (codeSigs (topBody p)) implSigs
    sigImpls =
      Map.fromList
        . map
          ( \impls ->
              ( Sig $ implSignature (head impls),
                Set.fromList $ map (Impl . implName) impls
              )
          )
        $ groupBy
          (\x y -> implSignature x == implSignature y)
          (Set.toList $ implementations p)

sigImpls :: ModularProgram -> Map SigNode (Set ImplNode)
sigImpls p =
  Map.fromList
    . map
      ( \impls ->
          let sig = SigNode $ implSignature (head impls) in
          ( sig,
            Set.fromList $ map (ImplNode (Just sig) . implName) impls
          )
      )
    . groupBy (\x y -> implSignature x == implSignature y)
    . sortOn implSignature
    . Set.toList
    $ implementations p

implSigs :: ModularProgram -> Map ImplNode (Set SigNode)
implSigs p = implSigs'
  where
    codeSigs = Set.map (\(sig, _, _) -> SigNode sig) . moduleInstances
    implSigs =
      Map.fromList
        . map
          ( \impl -> (ImplNode (Just . SigNode $ implSignature impl) $ implName impl, codeSigs (implBody impl))
          )
        $ (Set.toList $ implementations p)
    implSigs' = Map.insert (ImplNode Nothing "root") (codeSigs (topBody p)) implSigs

moduleTreeGraph :: ModularProgram -> Graph
moduleTreeGraph p =
  moduleGraphToDot
    (ModuleGraph allImpls allSigs (implSigs p) (sigImpls p))
  where
    allSigs = Set.map (\(_, sig) -> SigNode sig) (signatures p)
    allImpls = Set.insert (ImplNode Nothing "root") $ Set.map (\impl -> ImplNode (Just . SigNode $ implSignature impl) $ implName impl) (implementations p)

productSelectionSets :: [Set (Map SigNode ImplNode)] -> Set (Map SigNode ImplNode)
productSelectionSets = foldl1 multiplySelectionSets

multiplySelectionSets :: Set (Map SigNode ImplNode) -> Set (Map SigNode ImplNode) -> Set (Map SigNode ImplNode)
multiplySelectionSets s1 s2 = Set.fromList (liftA2 Map.union (Set.toList s1) (Set.toList s2))

-- productSels :: [Set (Map SigNode ImplNode)] -> Set (Map SigNode ImplNode)
-- productSels [] = Set.empty
-- productSels [s] = s
-- productSels (s1 : s2 : ss) = Set.union (productSels ss) $ Set.fromList (liftA2 Map.union (Set.toList s1) (Set.toList s2))

allImplSels :: Map ImplNode (Set SigNode) -> Map SigNode (Set ImplNode) -> ImplNode -> Set (Map SigNode ImplNode)
allImplSels iToS sToI impl = productSelectionSets (Set.toList sigMaps')
  where
    sigs = fromJust . Map.lookup impl $ iToS
    sigMaps = Set.map (allSigSels iToS sToI) sigs
    sigMaps' = if Set.null sigMaps then Set.singleton (Set.singleton Map.empty) else sigMaps

allSigSels :: Map ImplNode (Set SigNode) -> Map SigNode (Set ImplNode) -> SigNode -> Set (Map SigNode ImplNode)
allSigSels iToS sToI sig = Set.unions . Set.map (\(impl, implSels) -> Set.map (Map.insert sig impl) implSels) $ implMaps
  where
    impls = fromJust . Map.lookup sig $ sToI
    implMaps = Set.map (\impl -> (impl, allImplSels iToS sToI impl)) impls

allSelections :: ModularProgram -> Set (Map SigNode ImplNode)
allSelections p = allImplSels (implSigs p) (sigImpls p) (ImplNode Nothing "root")

offByOne :: Map SigNode ImplNode -> Map SigNode ImplNode -> Maybe (SigNode, ImplNode, ImplNode)
offByOne m1 m2 = case inters of
  [inter] -> Just inter
  _ -> Nothing
  where
    inters = catMaybes . Map.elems $ Map.intersectionWithKey (\s i1 i2 -> if i1 == i2 then Nothing else Just (s, i1, i2)) m1 m2

modelTreeGraph :: ModularProgram -> Graph
modelTreeGraph p = modelGraphToDot (ModelGraph allModels modelEdges)
  where
    -- allSel = zip (map show [1..]) . Set.toList $ allSelections p
    allSel = map (\sel -> (showSel sel, sel)) . Set.toList $ allSelections p
    allModels = Set.fromList . map (ModelNode . fst) $ allSel
    modelEdges =
      [ (ModelNode n1, ModelNode n2, DeltaModule s i1 i2)
        | (n1, n2, (SigNode s, ImplNode _ i1, ImplNode _ i2)) <-
            mapMaybe
              ( \((n1, s1), (n2, s2)) ->
                  (\x -> (n1, n2, x)) <$> offByOne s1 s2
              )
              ((,) <$> allSel <*> allSel)
      ]

showSels :: Set (Map SigNode ImplNode) -> String
showSels = intercalate "-------------------------\n" . map showSel . Set.toList

showSel :: Map SigNode ImplNode -> String
showSel = unlines . map (\(SigNode sig, ImplNode _ impl) -> sig ++ ": " ++ impl) . Map.toList

{-
writeup by June 1st:

cleaned mockup (maybe with a couple feature sets)
a few meaningful examples
collect various writing to say my current state of understanding
todo list
-}
