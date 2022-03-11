{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Macros where

import Types
import Data.List
import Data.Function
import Data.Maybe
import Parsing
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text


groupOn f = groupBy ((==) `on` f)

{-
Assumptions:
 - No macro will apply to the same hole twice
 - No macro will apply to the synthetic nodes produced by the same macro
 - Applying one macro will not cause another macro to apply that would not have applied before

Corollaries:
 - You can apply each macro to each hole once, straight through
-}

parseSel :: Selection -> Maybe UnexpendedSelection
parseSel = sequence . Map.map (parseUnexpandedImpls . unImplName)

expandSel :: MacroResult -> UnexpendedSelection -> Maybe Selection
expandSel r = maybeEncodeId . expandSelection r

contractSel :: MacroResult -> Selection -> UnexpendedSelection
contractSel r = contractSelection r . Map.map UXImpl

applyMacros :: [Macro] -> ModularProgram -> (ModularProgram, MacroResult)
applyMacros ms p = foldl accumMacro (p, mempty) ms
  where accumMacro (p', r) m = let (p'', r') = applyMacro m p' in (p'', r<>r')

applyMacro :: Macro -> ModularProgram -> (ModularProgram, MacroResult)
applyMacro m p =
  let result = mconcat $ map (idemMacro m) (groupOn implSignature (implementations p))
  in (p {implementations = newImpls result, signatures = signatures p ++ newSigs result}, result)

idemMacro :: Macro -> [ModuleImplementation ModularCode] -> MacroResult
idemMacro m impls = fromMaybe mempty {newImpls = impls} (m impls)

type Macro = [ModuleImplementation ModularCode] -> Maybe MacroResult

data MacroResult = MacroResult {
    newSigs :: [(Type, SigName)]
  , newImpls :: [ModuleImplementation ModularCode]
  , expandSelection :: UnexpendedSelection -> UnexpendedSelection
  , contractSelection :: UnexpendedSelection -> UnexpendedSelection
  }

maybeEncodeId :: UnexpendedSelection -> Maybe Selection
maybeEncodeId = sequence . Map.map maybeEncodeImplId
maybeEncodeImplId :: UnexpandedImpls -> Maybe ImplName
maybeEncodeImplId (UXImpl impl) = Just impl
maybeEncodeImplId _ = Nothing

collectionHoles :: Macro
collectionHoles [] = Nothing
collectionHoles impls@(headImpl@(implSignature -> sig@(parseCollectionHole . unSigName -> Just sig')):_) =
    Just $ MacroResult {
        newSigs = newSigs
      , newImpls = mergeImpl : noImpls ++ yesImpls
      , expandSelection = expandSelection
      , contractSelection = contractSelection
      }
  where sigNames = map (\comp -> (comp, SigName $ sig' <> "_" <> unImplName (implName comp))) impls
        contractSelection uxs =
          let collection = UXCollection . catMaybes . flip map sigNames $ \(comp, compSig) ->
                case Map.lookup compSig uxs of
                  Nothing -> error ""
                  Just (UXImpl (ImplName "yes")) -> Just (implName comp)
                  Just (UXImpl (ImplName "no")) -> Nothing
                  Just _ -> error "Collection synthetic hole had bad value"
          in Map.insert sig collection $ foldl (flip Map.delete) uxs (map snd sigNames)
        expandSelection uxs = case Map.lookup sig uxs of
          Nothing -> uxs
          Just (UXCollection compNames) ->
            let compSelections = Map.fromList (flip map sigNames $ \(comp, compSig) ->
                                                let compName' = if elem (implName comp) compNames
                                                      then ImplName "yes"
                                                      else ImplName "no"
                                                in (compSig, UXImpl compName'))
                mergeSelection = Map.singleton sig (UXImpl mergeName)
            in compSelections <> mergeSelection <> Map.delete sig uxs
          Just _ -> error "Should have given list for collection hole"
        newSigs = map (\(_, s) -> (Type "", s)) sigNames
        mergeName = (ImplName $ sig' <> "_merge")
        mergeImpl = ModuleImplementation mergeName mergeFields sig emptyBlocks
  -- module "collect_H*" H* {
  --   field f(..) {
  --     return append_array(H*1.f(..),
  --                         append_array(H*2.f(..),
  --                                      ..
  --                                      append_array(..,
  --                                                   H*N.f(..))));
  --   }
  -- }
        mergeFields = flip map (implFields headImpl) $ \field ->
          field { fieldBody = ModularCode {
            moduleInstances = Set.fromList . flip map sigNames $ \(_, compSig) ->
                (compSig, fieldSignature field, map Expr $ fieldArgs field)
          , modularCode =
            Code [] (Just . Expr . stanConcat . flip map sigNames $
                      \(_, SigName sig) ->
                        sig <> "(" <> Text.intercalate ", " (fieldArgs field) <> ")" )
          }}
  -- module "no" H*1 {
  --   field f(..) {
  --     array[0] type empty;
  --     return empty;
  --   }
  -- }
        noFields = flip map (implFields headImpl) $ \field ->
          field { fieldBody = ModularCode {
            moduleInstances = Set.empty
          , modularCode = Code ["array[0] type empty;"] (Just $ Expr "empty")
          }}
        noImpls = flip map sigNames $ \(_, sigName) ->
          ModuleImplementation (ImplName "no") noFields sigName emptyBlocks
        -- Just wrap the returns of all of the fields in {}
  -- module "yes" H*1 {
  --   field f(..) {
  --     array[1] type singleton;
  --     ..; //stmts of impl 1 of H*
  --     singleton[1] = ..; // return value of H*
  --     return singleton;
  --   }
  -- }
        -- yesFields comp = flip map (implFields comp) $ \field ->
        --   field { fieldBody = (fieldBody field) {
        --     modularCode =
        --     let varname = unImplName (implName comp) <> "_" <> "singleton" in
        --       Code {
        --           codeText = codeText (modularCode (fieldBody field)) ++ [
        --                 "array[1] type "<>varname<>";"
        --               , varname <> "[1] = " <> unExpr (fromJust (codeReturn (modularCode (fieldBody field)))) <> ";"
        --               ]
        --         , codeReturn = Just (Expr varname)
        --         }
        --   }}
        yesFields comp = flip map (implFields comp) $ \field ->
          field { fieldBody = (fieldBody field) {
            modularCode =
              Code {
                  codeText = codeText (modularCode (fieldBody field))
                , codeReturn = Just (Expr ("{ " <> unExpr (fromJust (codeReturn (modularCode (fieldBody field)))) <> " }"))
                }
          }}
        yesImpls = flip map sigNames $ \(comp, sigName) ->
          ModuleImplementation (ImplName "yes") (yesFields comp) sigName (implBlocks comp)
collectionHoles _ = Nothing

stanConcat :: [Text] -> Text
stanConcat = foldl1 (\acc a -> "append_array(" <> acc <> ", " <> a <> ")")


instance Semigroup MacroResult where
  a <> b = MacroResult {
      newSigs = newSigs a ++ newSigs b
    , newImpls = newImpls a ++ newImpls b
    , expandSelection = expandSelection a . expandSelection b
    , contractSelection = contractSelection b . contractSelection a
    }

instance Monoid MacroResult where
  mempty = MacroResult {
      newSigs = []
    , newImpls = []
    , expandSelection = id
    , contractSelection = id
    }
