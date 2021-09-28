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
selectModules :: ModularProgram -> Map SigName ImplName -> ConcreteProgram
selectModules program selectionNames = ConcreteProgram
    { concreteBody   = applyImplementations appliedSigImplementations
                                            (topBody program)
    , concreteParams = Set.union (topParams program) moduleParams
    , concreteGQ     =
        (applyImplementations appliedSigImplementations (topGQ program) <>)
        . foldl' (<>) mempty
        . catMaybes
        . map implGQ
        $ Map.elems appliedSigImplementations
    , concreteData   = topData program
    }
  where
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
                        (implementations program)
                    )
            in  case found of
                    Nothing ->
                        error
                            $  "Could not find implement \""
                            ++ Text.unpack implName'
                            ++ "\" of signature \""
                            ++ Text.unpack sigName'
                            ++ "\""
                    Just found -> found
        )
        selectionNames
    moduleParams              = Set.unions (Map.map implParams selections)
    applyOrderSigs            = topologicallyOrderSignatures selections
    appliedSigImplementations = foldl
        (\cImpls sigName ->
            let (Just mImpl) = Map.lookup sigName selections
                cImpl        = fmap (applyImplementations cImpls) mImpl
            in  Map.insert sigName cImpl cImpls
        )
        Map.empty
        applyOrderSigs

-- instanceInitializers
--     :: Map SigName ModuleImplementation -> Code -> Map InstanceName Code
-- instanceInitializers sigImpls code = undefined inits
--     where inits = moduleInstances code

{-
0. Assign arguments
1. Insert the code beforehand with arguments
2. Replace with return expression or nothing
-}

applyImplementations
    :: Map SigName (ModuleImplementation ConcreteCode)
    -> ModularCode
    -> ConcreteCode
applyImplementations sigImpls code = ConcreteCode $ Set.fold
    (\(sigName, _, exprs) ccode ->
        let impl = case Map.lookup sigName sigImpls of
                Just impl -> impl
                Nothing ->
                    error
                        $  "can't find signature "
                        ++ Text.unpack sigName
                        ++ "\n"
                        ++ (unlines $ [show sigImpls, show code])
        in  applyImplementation sigName
                                exprs
                                (implArgs impl)
                                (implBody impl)
                                ccode
    )
    (modularCode code)
    (moduleInstances code)

applyImplementation
    :: SigName -> [Expr] -> [Symbol] -> ConcreteCode -> Code -> Code
applyImplementation sigName args argNames (ConcreteCode implBody) code = code
    { codeText   = codeText' <> returnPrepLines
    , codeReturn = codeReturn'
    }
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
    insertByLine :: Text -> ([Text], Maybe Text)
    insertByLine line = case parseSigLine sigName line of
        Nothing -> ([], Just line)
        Just (_, rebuildLine) ->
            ( assignmentLines <> codeText implBody -- set arguments here?
            , maybe Nothing
                    (\return -> Just (rebuildLine return))
                    (codeReturn implBody)
            )
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

topologicallyOrderSignatures
    :: Map SigName (ModuleImplementation ModularCode) -> [SigName]
topologicallyOrderSignatures impls = reverse sortedSigs
  where
    children = map (\(sigName, _, _) -> sigName) . Set.toList . moduleInstances
    (graph, fromVertex, _) =
        Graph.graphFromEdges
            . map
                  (\(sigName, impl) ->
                      (sigName, sigName, children (implBody impl))
                  )
            . Map.toList
            $ impls
    vertices = Graph.topSort graph
    sortedSigs =
        map (\v -> let (sigName, _, _) = fromVertex v in sigName) vertices


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
                            (ImplNode Nothing "root")
                            (implSigs p)
                            (sigImpls p)
                            (\(ImplNode _ a) -> "[" <> a <> "]")
                            (\(SigNode a) -> "{" <> a <> "}")
  -- where
    -- sigs = Map.insert (ImplNode Nothing "root") (codeSigs (topBody p)) implSigs
    -- codeSigs = Set.map (\(sig, _, _) -> SigNode sig) . moduleInstances

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

sigImpls :: ModularProgram -> Map SigNode (Set ImplNode)
sigImpls p =
    Map.fromList
        . map
              (\impls ->
                  let sig = SigNode $ implSignature (head impls)
                  in  ( sig
                      , Set.fromList
                          $ map (ImplNode (Just sig) . implName) impls
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
                  (\impl ->
                      ( ImplNode (Just . SigNode $ implSignature impl)
                          $ implName impl
                      , codeSigs (implBody impl)
                      )
                  )
            $ (Set.toList $ implementations p)
    implSigs' =
        Map.insert (ImplNode Nothing "root") (codeSigs (topBody p)) implSigs

moduleTreeGraph :: ModularProgram -> Graph
moduleTreeGraph p = moduleGraphToDot
    (ModuleGraph allImpls allSigs (implSigs p) (sigImpls p))
  where
    allSigs  = Set.map (\(_, sig) -> SigNode sig) (signatures p)
    allImpls = Set.insert (ImplNode Nothing "root") $ Set.map
        (\impl -> ImplNode (Just . SigNode $ implSignature impl) $ implName impl
        )
        (implementations p)

productSelectionSets
    :: [Set (Map SigNode ImplNode)] -> Set (Map SigNode ImplNode)
productSelectionSets = foldl1 multiplySelectionSets

multiplySelectionSets
    :: Set (Map SigNode ImplNode)
    -> Set (Map SigNode ImplNode)
    -> Set (Map SigNode ImplNode)
multiplySelectionSets s1 s2 =
    Set.fromList (liftA2 Map.union (Set.toList s1) (Set.toList s2))

-- productSels :: [Set (Map SigNode ImplNode)] -> Set (Map SigNode ImplNode)
-- productSels [] = Set.empty
-- productSels [s] = s
-- productSels (s1 : s2 : ss) = Set.union (productSels ss) $ Set.fromList (liftA2 Map.union (Set.toList s1) (Set.toList s2))

allImplSels
    :: Map ImplNode (Set SigNode)
    -> Map SigNode (Set ImplNode)
    -> ImplNode
    -> Set (Map SigNode ImplNode)
allImplSels iToS sToI impl = productSelectionSets (Set.toList sigMaps')
  where
    sigs     = fromJust . Map.lookup impl $ iToS
    sigMaps  = Set.map (allSigSels iToS sToI) sigs
    sigMaps' = if Set.null sigMaps
        then Set.singleton (Set.singleton Map.empty)
        else sigMaps

allSigSels
    :: Map ImplNode (Set SigNode)
    -> Map SigNode (Set ImplNode)
    -> SigNode
    -> Set (Map SigNode ImplNode)
allSigSels iToS sToI sig =
    Set.unions
        . Set.map (\(impl, implSels) -> Set.map (Map.insert sig impl) implSels)
        $ implMaps
  where
    impls    = fromJust . Map.lookup sig $ sToI
    implMaps = Set.map (\impl -> (impl, allImplSels iToS sToI impl)) impls

allSelections :: ModularProgram -> Set (Map SigNode ImplNode)
allSelections p =
    allImplSels (implSigs p) (sigImpls p) (ImplNode Nothing "root")

offByOne
    :: Map SigNode ImplNode
    -> Map SigNode ImplNode
    -> Maybe (SigNode, ImplNode, ImplNode)
offByOne m1 m2 = case inters of
    [inter] -> Just inter
    _       -> Nothing
  where
    inters = catMaybes . Map.elems $ Map.intersectionWithKey
        (\s i1 i2 -> if i1 == i2 then Nothing else Just (s, i1, i2))
        m1
        m2

selectionToNodes :: Selection -> Map SigNode ImplNode
selectionToNodes = Map.mapKeys SigNode . Map.mapWithKey (\sig impl -> ImplNode (Just (SigNode sig)) impl)

nodesToSelection :: Map SigNode ImplNode -> Selection
nodesToSelection = Map.mapKeys (\(SigNode sig) -> sig) . Map.map (\(ImplNode _ impl) -> impl)

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
        | (n1, n2, (SigNode s, ImplNode _ i1, ImplNode _ i2)) <- mapMaybe
            (\((n1, s1), (n2, s2)) -> (\x -> (n1, n2, x)) <$> offByOne s1 s2)
            ((,) <$> allSel <*> allSel)
        ]

arbitrarySelection :: ModularProgram -> Selection
arbitrarySelection = nodesToSelection . Set.findMin . allSelections

modelNeighbors :: ModularProgram -> Selection -> Set Selection
modelNeighbors prog s1 = Set.map nodesToSelection . Set.filter (\s2 -> isJust (selectionToNodes s1 `offByOne` s2)) $ allSelections prog

showSels :: Set (Map SigNode ImplNode) -> Text
showSels =
    Text.intercalate "-------------------------\n" . map showSel . Set.toList

showSel :: Map SigNode ImplNode -> Text
showSel =
    Text.unlines
        . map (\(SigNode sig, ImplNode _ impl) -> sig <> ": " <> impl)
        . Map.toList


{-
writeup by June 1st:

cleaned mockup (maybe with a couple feature sets)
a few meaningful examples
collect various writing to say my current state of understanding
todo list
-}
