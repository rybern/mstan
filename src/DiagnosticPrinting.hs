{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module DiagnosticPrinting where

import Indent

import Types

import           Data.Maybe
import           Data.Text                      ( Text )

import qualified Data.Text.IO                  as Text
import qualified Data.Set                      as Set

fieldLines :: ModuleField ModularCode -> [Text]
fieldLines (ModuleField {..}) = concat $ catMaybes
  [ Just ["fieldBody:"]
  , Just  . indent 1 . linesModularCode $ fieldBody
  , Just ["fieldArgs:"]
  , Just . indent 1 $ fieldArgs
  , (("fieldSignature:" :) . indent 1 . return . unFieldName) <$> fieldSignature
  ]

implementationLines :: ModuleImplementation ModularCode -> [Text]
implementationLines (ModuleImplementation {..}) = concat $ catMaybes
  [ Just ["implName:"]
  , Just . indent 1 $ [unImplName implName]
  , Just ["implFields:"]
  , Just . concat . map (starIndent 2 . fieldLines) $ implFields
  , (("sigName:" :) <$>) . whenNonempty . indent 1 $ [unSigName implSignature]
  , (("implFunctions:" :) . indent 1 . linesModularCode) <$> functions >>= whenNonempty
  , (("implTD:" :) . indent 1 . linesModularCode) <$> td >>= whenNonempty
  , (("implGQ:" :) . indent 1 . linesModularCode) <$> gq >>= whenNonempty
  , (("implParams:" :) <$>) . whenNonempty . indent 1 . map unParam . Set.toList $ params
  ]
  where (Blocks {..}) = implBlocks

printModularProgram :: ModularProgram -> IO ()
printModularProgram = mapM_ Text.putStrLn . modularProgramLines

moduleInstanceLines :: (SigName, Maybe FieldName, [Expr]) -> [Text]
moduleInstanceLines (SigName sigName, mField, args) = concat $ catMaybes
  [ Just ["signature:"]
  , Just $ indent 1 [sigName]
  , (\(FieldName fieldName) -> ["field name:"] ++ (indent 1 [fieldName])) <$> mField
  , Just ["args:"]
  , Just $ indent 1 $ map unExpr args
  ]

linesModularCode :: ModularCode -> [Text]
linesModularCode (ModularCode {..}) = concat $ catMaybes
  [ Just ["code:"]
  , Just . indent 1 $ codeText modularCode
  , (("return:" :) . indent 1 . return . unExpr) <$> codeReturn modularCode
  , Just ["modules:"]
  , Just . concat . map (starIndent 2 . moduleInstanceLines) . Set.toList $ moduleInstances
  ]

modularProgramLines :: ModularProgram -> [Text]
modularProgramLines (ModularProgram {..}) = concat $ catMaybes
  [ Just ["signatures:"]
  , Just . indent 1 . map (unSigName . snd) . Set.toList $ signatures
  , Just ["implementations:"]
  , Just . concat . map (starIndent 2 . implementationLines) $ implementations
  , (("topFunctions:" :) . indent 1 . linesModularCode) <$> functions >>= whenNonempty
  , (("topData:" :) <$>) . whenNonempty . indent 1 $ progData topProgram
  , (("topTD:" :) . indent 1 . linesModularCode) <$> td >>= whenNonempty
  , (("topParams:" :) <$>) . whenNonempty . indent 1 . map unParam . Set.toList $ params
  , (("topBody:" :) <$>) . whenNonempty =<< indent 1 . linesModularCode <$> model
  , (("topGQ:" :) . indent 1 . linesModularCode) <$> gq >>= whenNonempty
  ]
  where (Blocks {..}) = progBlocks topProgram


-- linesConcreteProgram :: ConcreteProgram -> [Text]
-- linesConcreteProgram (ConcreteProgram {..}) = concat $ catMaybes
--   [ (linesBlock "functions" <$>) . whenNonempty . linesCode $ concreteFunctions
--   , Just . linesBlock "data" $ concreteData
--   , (linesBlock "transformed data" <$>) . whenNonempty . linesCode $ concreteTD
--   , Just . linesBlock "parameters" . map lineParam . Set.toList $ concreteParams
--   , Just . linesBlock "model" . linesCode $ concreteBody
--   , (linesBlock "generated quantities" <$>) . whenNonempty . linesCode $ concreteGQ
--   ]
