{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ModularStan where

import           Algebra.Graph
import           Algebra.Graph.Undirected (neighbours, toUndirected)

import qualified Data.Graph               as Graph
import           Data.List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Parsing
import           ToGraph
import           Types


------
-- Concrete program generation
------

-- Constraint:
-- The selection map is total for all signatures in the program
selectModules :: ModularProgram -> Map SigName ImplName -> Program ConcreteCode
selectModules
  (ModularProgram {..}) selectionNames =
  Program
    { progBlocks    =
        mconcat $ fmap concretizeCode (progBlocks topProgram) : map implBlocks concreteModules
    , progData      = progData topProgram
    }
  where
    -- User's mapping from module signatures (the bases, sometimes on the LHS of the '.') to modules
    selections = Map.mapWithKey
        (\sigName' implName' ->
            let
                found =
                    (find
                        (\impl ->
                            implName impl
                                == implName'
                                && implSignature impl
                                == sigName'
                        )
                        implementations
                    )
            in  case found of
                    Nothing ->
                        error
                            $  "Could not find implementation \""
                            ++ Text.unpack (unImplName implName')
                            ++ "\" of signature \""
                            ++ Text.unpack (unSigName sigName')
                            ++ "\""
                    Just found -> found
        )
        selectionNames

    -- Mapping from field signature (the whole A.B) to (modular) field implementations
    implementationMap = splitModulesIntoFields selections
    -- Order that signatures should be concretized to avoid missing dependencies
    applyOrderSigs = topologicallyOrderSignatures implementationMap
    -- Mapping from field signature to concretized field implementations
    concretizedFieldImplementations = foldl
        (\cImpls sigName ->
            let (Just mImpl) = Map.lookup sigName implementationMap
                cImpl        = fmap (applyImplementations cImpls) mImpl
            in  Map.insert sigName cImpl cImpls
        )
        Map.empty
        applyOrderSigs

    -- Apply all of the field signatures in a piece of code
    concretizeCode :: ModularCode -> ConcreteCode
    concretizeCode  = applyImplementations concretizedFieldImplementations

    concreteModules = map (concretizeCode <$>) $ Map.elems selections

{-
0. Assign arguments
1. Insert the code beforehand with arguments
2. Replace with return expression or nothing
-}

orderModuleApplication :: Set (SigName, Maybe FieldName, [Expr]) -> Maybe [(SigName, Maybe FieldName, [Expr])]
orderModuleApplication set = Just sortedApplications
  where
    toFullSig (sig, field, _) = fullSigName (sig, field)
    signatures = Set.toList $ Set.map toFullSig set
    -- The list of signatures that an application's arguments depend on
    children (_, _, args) = flip concatMap args $ \(Expr arg) ->
      filter (isJust . flip parseSigLine arg) signatures
    (graph, fromVertex, _) =
        Graph.graphFromEdges
            . map
                  (\app ->
                      (app, toFullSig app, children app)
                  )
            $ Set.toList set
    vertices = Graph.topSort graph
    sortedApplications =
        reverse . map ((\(app, _, _) -> app) . fromVertex) $ vertices

applyImplementations
    :: Map FullSigName (ModuleField ConcreteCode) -> ModularCode -> ConcreteCode
applyImplementations sigImpls code =
  ConcreteCode $ foldl
    (\ccode (sigName, fieldName, exprs) ->
        let fullSig = fullSigName (sigName, fieldName)
            impl = case Map.lookup fullSig sigImpls of
                Just impl -> impl
                Nothing ->
                    error
                        $  "can't find signature "
                        ++ Text.unpack (unFullSigName fullSig)
                        ++ "\n"
                        ++ (unlines $ [show sigImpls, show code])
        in  applyImplementation (argumentStrategy (fieldArgs impl))
                                fullSig
                                (fieldBody impl)
                                ccode
    )
    (modularCode code)
    order
  where (order, argumentStrategy) = case orderModuleApplication (moduleInstances code) of
          Nothing -> (Set.toList $ moduleInstances code, argumentAssignmentStrategy)
          Just order -> (order, argumentPropagationStrategy)

-- When we can topologically sort module application by argument structure, we can propagate expressions. Otherwise we have to make a new variable for each argument.

argumentAssignmentStrategy :: [Symbol] -> [Expr] -> Code -> Code
argumentAssignmentStrategy argNames args implCode =
  implCode { codeText = assignmentLines <> codeText implCode }
  where
    assignmentLines =
        map (\(argName, Expr arg) -> argName <> " = " <> arg <> ";")
            . filter
                  (\(argName, Expr arg) -> (last . Text.words $ argName) /= arg)
            $ (zip
                  (if length argNames > length args
                      then tail argNames
                      else argNames
                  )
                  args
              )

-- We need to wrap the expression in () sometimes
-- Needed for anything other than parentheticals, vars, or juxtapositions thereof
argumentPropagationStrategy :: [Symbol] -> [Expr] -> Code -> Code
argumentPropagationStrategy argNames args implCode = Code
  { codeReturn = Expr . replaceAllArgs . unExpr <$> codeReturn implCode
  , codeText = map replaceAllArgs $ codeText implCode }
  where replaceAllArgs line = foldl
                 (\line (argName, arg) -> snd . replaceCountVar argName (unExpr . maybeAddParens $ arg) $ line)
                 line
                 $ zip argNames args

applyImplementation
    :: ([Expr] -> Code -> Code) -> FullSigName -> ConcreteCode -> Code -> Code
applyImplementation argumentStrategy fullSig (ConcreteCode implCode) code = code
    { codeText   = codeText' <> returnPrepLines
    , codeReturn = codeReturn'
    }
  where
    -- We probably actually need to parse the same line multiple times to find more than one instance
    insertByLine :: Text -> ([Text], Maybe Text)
    insertByLine line = case parseSigLine fullSig line of
        -- No signature found
        Nothing -> ([], Just line)
        Just (args, maybeRebuildWithExpression) ->
         let inlinedImplCode = argumentStrategy args implCode
             rebuiltLine = case (maybeRebuildWithExpression, codeReturn inlinedImplCode) of
               -- Signature being used as a statement, impl has no return
               (Nothing, Nothing) -> Nothing
               -- Signature being used as an expression, impl has return
               (Just rebuildLine, Just returnExpr) -> Just (rebuildLine returnExpr)
               -- Signature being used as a statement, but impl has return
               (Nothing, Just _) ->
                 error $ concat
                        [ "Type error: in line \""
                        , Text.unpack line
                        , "\" signature "
                        , Text.unpack (unFullSigName fullSig)
                        , " is used as though it does not return an expression, but it does."
                        ]
               -- Signature being used as an expression, but impl has no return
               (Just _, Nothing) ->
                 error $ concat
                        [ "Type error: in line \""
                        , Text.unpack line
                        , "\" signature "
                        , Text.unpack (unFullSigName fullSig)
                        , " is used as though it returns an expression, but it does not."
                        ]
         in (codeText inlinedImplCode, rebuiltLine)
    -- perform the replacement on the return expression
    -- the return expression won't be identified as a statement because it has no semicolon
    (returnPrepLines, codeReturn') =
        case (insertByLine . unExpr) <$> codeReturn code of
            Nothing -> ([], Nothing)
            Just (returnPrepLines, codeReturn') ->
                (returnPrepLines, Expr <$> codeReturn')
    codeText' =
        filter (/= "")
            . concatMap
                  (\line -> case insertByLine line of
                      (prepLines, Nothing      ) -> prepLines
                      (prepLines, Just thisLine) -> prepLines ++ [thisLine]
                  )
            $ codeText code

splitModulesIntoFields
    :: Map SigName (ModuleImplementation ModularCode)
    -> Map FullSigName (ModuleField ModularCode)
splitModulesIntoFields =
    Map.fromList . concatMap splitModuleIntoList . Map.toList
  where
    splitModuleIntoList (moduleSig, moduleImpl) =
        [ (fullSigName (moduleSig, fieldSignature field), field) | field <- implFields moduleImpl ]

topologicallyOrderSignatures
    :: Map FullSigName (ModuleField ModularCode) -> [FullSigName]
topologicallyOrderSignatures impls = sortedSigs
  where
    children = map (\(sigName, fieldName, _) -> fullSigName (sigName, fieldName)) . Set.toList . moduleInstances
    (graph, fromVertex, _) =
        Graph.graphFromEdges
            . map
                  (\(sigName, impl) ->
                      (sigName, sigName, children (fieldBody impl))
                  )
            $ Map.toList impls
    vertices = Graph.topSort graph
    sortedSigs =
        reverse . map ((\(sigName, _, _) -> sigName) . fromVertex) $ vertices

------
-- Data structure manipulation for graph/tree abstractions
------

-- Mapping from each signatures to all of its implementations
sigImpls :: ModularProgram -> Map SigName (Set ImplName)
sigImpls p =
    Map.fromList
        . map
              (\impls ->
                  let sig = implSignature (head impls)
                  in  ( sig
                      , Set.fromList
                          $ map (implName) impls
                      )
              )
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
    implSigs =
        Map.fromList
            . map
                  (\impl ->
                      ( (Just (implSignature impl), implName impl)
                      , moduleSigs impl
                      )
                  )
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

idToSel :: ImplID -> Selection
idToSel (Nothing, _)     = Map.empty
idToSel (Just sig, impl) = Map.singleton sig impl

treeModelNetwork :: ModuleTree -> Graph Selection
treeModelNetwork (SigTree impls) = foldl1' connect $ map treeModelNetwork impls
treeModelNetwork (ImplTree name []) = Vertex (idToSel name)
treeModelNetwork (ImplTree name sigs) =
  fmap (idToSel name <>) . foldl1' box' $ map treeModelNetwork sigs
  where box' g1 g2 = (\(a, b) -> a <> b) <$> box g1 g2


------
-- Model graph interface
------

modelNetwork :: ModularProgram -> Graph Selection
modelNetwork = treeModelNetwork . moduleTree

allSelections :: ModularProgram -> Set Selection
allSelections = vertexSet . modelNetwork

arbitrarySelection :: ModularProgram -> Selection
arbitrarySelection = Set.findMin . allSelections

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors p s = neighbours s . toUndirected $ modelNetwork p


------
-- Visualizations
------

-- Node- and Edge-set representation of the network of models for visualization
modelGraph :: ModularProgram -> ModelGraph
modelGraph p = ModelGraph allModels modelEdges
  where
    network = modelNetwork p
    toNode = ModelNode . showSel
    allModels = Set.map toNode (vertexSet network)
    modelEdges = map edgeDelta . Set.toList $ edgeSet network

    selectionDeltas :: Selection -> Selection -> [DeltaModule]
    selectionDeltas a b =
        filter (\(DeltaModule _ i1 i2) -> i1 /= i2)
      . Map.elems
      $ Map.intersectionWithKey (\(SigName sig) (ImplName impl1) (ImplName impl2) ->
                                    DeltaModule sig impl1 impl2) a b

    edgeDelta :: (Selection, Selection) -> (ModelNode, ModelNode, DeltaModule)
    edgeDelta (a, b) = case selectionDeltas a b of
      [delta] -> (toNode a, toNode b, delta)
      ds -> error $ "Internal issue: neighboring selections differ by !=1 key. " ++ show ds

showSel :: Selection -> Text
showSel =
    Text.unlines
        . map (\(SigName sig, ImplName impl) -> sig <> ": " <> impl)
        . Map.toList

modelGraphviz :: ModularProgram -> Graphviz
modelGraphviz p = modelGraphToDot (modelGraph p)

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
