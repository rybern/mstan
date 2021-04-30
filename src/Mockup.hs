{-# LANGUAGE EmptyDataDeriving #-}
module Mockup where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List

type Symbol = String

-- Stan-ish types
newtype Type = Type String deriving (Eq, Ord)
newtype Expr = Expr String deriving (Eq, Ord)
data Code = Code {
    moduleReferences :: Set (InstanceName, Reference)
  , moduleInstances :: Set (SigName, Maybe InstanceName, [Expr])
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
data ModularProgram = ModularProgram {
    signatures :: Set (Type, SigName) -- Constraints: Unique SigName
  , implementations :: Set ModuleImplementation
  , topBody :: Code
  , topParams :: Set Param
  }

data ModuleImplementation = ModuleImplementation {
    implBody :: Code
  , implArgs :: [Symbol]
  , implSignature :: SigName
  , implParams :: Set Param
  , implName :: ImplName
  } deriving (Eq, Ord)

data ConcreteProgram = ConcreteProgram {
    concreteBody :: Code
  , concreteParams :: Set Param
  }

-- Constraint:
-- The selection map is total for all signatures in the program
applyModuleSelections
    :: ModularProgram -> Map SigName ImplName -> ConcreteProgram
applyModuleSelections program selectionNames = ConcreteProgram
    { concreteBody   = applyModuleReferences appliedSigImplementations
                                             (topBody program)
    , concreteParams = Set.union (topParams program) moduleParams
    }
  where
    selections = Map.map
        (\name ->
            fromJust
                (find (\impl -> name == implName impl) (implementations program)
                )
        )
        selectionNames
    (sigImplementations, moduleParams) =
        selectImplementations selections (implementations program)
    applyModules = applyModuleReferences sigImplementations
    applyOrderSigs = (topologicallyOrderSignatures sigImplementations)
    appliedSigImplementations = foldl
        (\impls sigName -> Map.update
            (\_ -> Just
                (\args -> applyModuleReferences
                    impls
                    ((fromJust (Map.lookup sigName impls)) args)
                )
            )
            sigName
            impls
        )
        sigImplementations
        applyOrderSigs

selectImplementations
    :: Map SigName ModuleImplementation
    -> Set ModuleImplementation
    -> (Map SigName ([Expr] -> Code), Set Param)
selectImplementations = undefined

instanceInitializers
    :: Map SigName ([Expr] -> Code) -> Code -> Map InstanceName Code
instanceInitializers sigImpls code = undefined inits
    where inits = moduleInstances code

applyModuleReferences :: Map SigName ([Expr] -> Code) -> Code -> Code
applyModuleReferences sigImpls code = Set.fold applyReference
                                               code
                                               translatedReferences
  where
    instances            = instanceInitializers sigImpls code
    references           = moduleReferences code
    translatedReferences = Set.map
        (\(instanceName, reference) ->
            (fromJust (Map.lookup instanceName instances), reference)
        )
        references

-- moduleReferences :: Code -> Set (InstanceName, Reference)
-- moduleReferences =
--   undefined

-- moduleInstances :: Code -> Set (SigName, InstanceName, [Expr])
-- moduleInstances = undefined

applyReference :: (Code, Reference) -> Code -> Code
applyReference = undefined

topologicallyOrderSignatures :: Map SigName ([Expr] -> Code) -> [SigName]
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
type ModuleApplication
    = ModularProgram -> Map SigName ImplName -> ConcreteProgram
-- Module reference
type ModuleReference = Reference
-- Module selection
type ModuleSelection = (SigName, ImplName)

data Sig = Sig String deriving (Show, Eq, Ord)
data Impl = Impl String deriving (Show, Eq, Ord)

drawTreeLevel :: Int -> a -> (a -> String) -> IO ()
drawTreeLevel level a showA = putStrLn $ (replicate level ' ') ++ showA a

drawTree
    :: (Ord a, Ord b)
    => Int
    -> a
    -> Map a (Set b)
    -> Map b (Set a)
    -> (a -> String)
    -> (b -> String)
    -> IO ()
drawTree level root next other showA showB = do
    drawTreeLevel level root showA
    let nexts = case Map.lookup root next of
          Just nexts -> nexts
          Nothing -> Set.empty
    mapM_ (\n -> drawTree (level + 2) n other next showB showA) nexts

drawModuleTree :: ModularProgram -> IO ()
drawModuleTree p = drawTree 0 (Impl "root") sigs sigImpls (\(Impl a) -> "[" ++ a ++ "]") (\(Sig a) -> "{" ++ a ++ "}")
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
                  (\impls ->
                      ( Sig $ implSignature (head impls)
                      , Set.fromList $ map (Impl . implName) impls
                      )
                  )
            $ groupBy (\x y -> implSignature x == implSignature y)
                      (Set.toList $ implementations p)
