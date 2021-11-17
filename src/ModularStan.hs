{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module ModularStan where

import           Control.Applicative
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Graph                    as Graph
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           ToGraph
import           Types
import           Parsing




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

-- instanceInitializers
--     :: Map SigName ModuleImplementation -> Code -> Map InstanceName Code
-- instanceInitializers sigImpls code = undefined inits
--     where inits = moduleInstances code

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

-- Set.fold applyInit
-- code
-- (moduleInstances code)
-- where modules =

-- applyInit
--     :: Code
--     -> (SigName, Maybe InstanceName, [Expr])
--     -> Map SigName ModuleImplementation
--     -> Code
-- applyInit code (sigName, _, argnames) map = code
--     where Just defn = Map.lookup sigName map

-- withArgs = (. map (\(argName, arg) -> "argName = arg;"). unlines $  ) ++ codeText defn

-- moduleReferences :: Code -> Set (InstanceName, Reference)
-- moduleReferences =
--   undefined

-- moduleInstances :: Code -> Set (SigName, InstanceName, [Expr])
-- moduleInstances = undefined

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


-- Nouns:

-- Module signature
type ModuleSignature = (Type, SigName)

-- Module instantiation
type ModuleInstantiation = (SigName, Maybe FieldName, [Expr])

-- Module implementation
type ModuleImplementation' = ModuleImplementation

-- Modular program
type ModularProgram' = ModularProgram

-- Concrete program/model
type ConcreteProgram' = Program ConcreteCode

-- Parameter set
type ParameterSet = Set Param

-- Module application
type ModuleApplication
    = ModularProgram -> Map SigName ImplName -> Program ConcreteCode

-- Module selection
type ModuleSelection = (SigName, ImplName)

data Sig = Sig Text deriving (Show, Eq, Ord)

data Impl = Impl Text deriving (Show, Eq, Ord)

drawTreeLevel :: Int -> a -> (a -> Text) -> IO ()
drawTreeLevel level a showA =
    Text.putStrLn $ (Text.replicate level " ") <> showA a

drawTree
    :: (Ord a, Ord b)
    => Int
    -> a
    -> Map a (Set b)
    -> Map b (Set a)
    -> (a -> Text)
    -> (b -> Text)
    -> IO ()
drawTree level root next other showA showB = do
    drawTreeLevel level root showA
    let nexts = case Map.lookup root next of
            Just nexts -> nexts
            Nothing    -> Set.empty
    mapM_ (\n -> drawTree (level + 2) n other next showB showA) nexts

drawModuleTree :: ModularProgram -> IO ()
drawModuleTree p = drawTree 0
                            (Nothing, ImplName "root")
                            (implSigs p)
                            (sigImplsAddImplIDs $ sigImpls p)
                            (\(_, ImplName a) -> "[" <> a <> "]")
                            (\(SigName a) -> "{" <> a <> "}")
  -- where
    -- sigs = Map.insert (ImplNode Nothing "root") (codeSigs (topBody p)) implSigs
    -- codeSigs = Set.map (\(sig, _, _) -> SigName sig) . moduleInstances

    -- implSigs =
    --     Map.fromList
    --         . map (\impl -> (Impl $ implName impl, codeSigs (implBody impl)))
    --         $ (Set.toList $ implementations p)
    -- sigImpls =
    --     Map.fromList
    --         . map
    --               (\impls ->
    --                   ( Sig $ implSignature (head impls)
    --                   , Set.fromList $ map (Impl . implName) impls
    --                   )
    --               )
    --         $ groupBy (\x y -> implSignature x == implSignature y)
    --                   (Set.toList $ implementations p)

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

moduleTreeGraph :: ModularProgram -> Graph
moduleTreeGraph p = moduleGraphToDot
    (ModuleGraph allImpls allSigs (implSigs p) (sigImpls p))
  where
    allSigs  = Set.map (\(_, SigName sig) -> SigName sig) (signatures p)
    allImpls = (Nothing, ImplName "root") : map
        (\impl -> (Just (implSignature impl), implName impl))
        (implementations p)

productSelectionSets
    :: [Set Selection] -> Set Selection
productSelectionSets = foldl1 multiplySelectionSets

multiplySelectionSets
    :: Set Selection
    -> Set Selection
    -> Set Selection
multiplySelectionSets s1 s2 =
    Set.fromList (liftA2 Map.union (Set.toList s1) (Set.toList s2))

-- productSels :: [Set (Map SigName ImplNode)] -> Set (Map SigName ImplNode)
-- productSels [] = Set.empty
-- productSels [s] = s
-- productSels (s1 : s2 : ss) = Set.union (productSels ss) $ Set.fromList (liftA2 Map.union (Set.toList s1) (Set.toList s2))

allImplSels
    :: Map ImplID (Set SigName)
    -> Map SigName (Set ImplName)
    -> ImplID
    -> Set Selection
allImplSels iToS sToI impl = productSelectionSets (Set.toList sigMaps')
  where
    sigs     = fromJust . Map.lookup impl $ iToS
    sigMaps  = Set.map (allSigSels iToS sToI) sigs
    sigMaps' = if Set.null sigMaps
        then Set.singleton (Set.singleton Map.empty)
        else sigMaps

allSigSels
    :: Map ImplID (Set SigName)
    -> Map SigName (Set ImplName)
    -> SigName
    -> Set Selection
allSigSels iToS sToI sig = Set.map (Map.map snd) $
    Set.unions
        . Set.map (\(impl, implSels) -> Set.map (Map.insert sig impl) implSels)
        $ implMaps
  where
    impls    = Set.map (Just sig,) . fromJust . Map.lookup sig $ sToI
    implMaps = Set.map (\impl -> (impl, Set.map selectionAddImplIDs $ allImplSels iToS sToI impl)) impls

allSelections :: ModularProgram -> Set Selection
allSelections p =
    allImplSels (implSigs p) (sigImpls p) (Nothing, ImplName "root")

offByOne
    :: Selection
    -> Selection
    -> Maybe (SigName, ImplName, ImplName)
offByOne m1 m2 = case inters of
    [inter] -> Just inter
    _       -> Nothing
  where
    inters = catMaybes . Map.elems $ Map.intersectionWithKey
        (\s i1 i2 -> if i1 == i2 then Nothing else Just (s, i1, i2))
        m1
        m2

sigImplsAddImplIDs :: Map SigName (Set ImplName) -> Map SigName (Set ImplID)
sigImplsAddImplIDs = Map.mapWithKey (\sig impls -> Set.map (Just sig,) impls)

selectionAddImplIDs :: Selection -> Map SigName ImplID
selectionAddImplIDs = Map.mapWithKey (\sig impl -> (Just sig, impl))

-- nodesToSelection :: Map SigName ImplNode -> Selection
-- nodesToSelection =
--     Map.mapKeys (\(SigName sig) -> SigName sig) . Map.map (\(ImplNode _ impl) -> ImplName impl)

modelTreeGraph :: ModularProgram -> Graph
modelTreeGraph p = modelGraphToDot (modelTree p)

modelTree :: ModularProgram -> ModelGraph
modelTree p = (ModelGraph allModels modelEdges)
  where
    -- allSel = zip (map show [1..]) . Set.toList $ allSelections p
    allSel    = map (\sel -> (showSel sel, sel)) . Set.toList $ allSelections p
    allModels = Set.fromList . map (ModelNode . fst) $ allSel
    modelEdges =
        [ (ModelNode n1, ModelNode n2, DeltaModule s i1 i2)
        | (n1, n2, (SigName s, ImplName i1, ImplName i2)) <- mapMaybe
            (\((n1, s1), (n2, s2)) -> (\x -> (n1, n2, x)) <$> offByOne s1 s2)
            ((,) <$> allSel <*> allSel)
        ]

arbitrarySelection :: ModularProgram -> Selection
arbitrarySelection = Set.findMin . allSelections

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors prog s1 =
        Set.filter (\s2 -> isJust (s1 `offByOne` s2))
        $ allSelections prog

showSels :: Set Selection -> Text
showSels =
    Text.intercalate "-------------------------\n" . map showSel . Set.toList

showSel :: Selection -> Text
showSel =
    Text.unlines
        . map (\(SigName sig, ImplName impl) -> sig <> ": " <> impl)
        . Map.toList


{-
writeup by June 1st:

cleaned mockup (maybe with a couple feature sets)
a few meaningful examples
collect various writing to say my current state of understanding
todo list
-}
