{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ConcreteProgram where

import qualified Data.Graph               as Graph
import           Data.List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text

import           Parsing

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
