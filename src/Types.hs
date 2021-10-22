{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
module Types where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

newtype MStanFile = MStanFile { unMStanFile :: FilePath }
  deriving Show

type Symbol = Text

-- Stan-ish types
newtype Type = Type Text deriving (Eq, Ord, Show)

newtype Expr = Expr {unExpr :: Text} deriving (Eq, Ord, Show)

data Code = Code
  { codeText :: [Text],
    codeReturn :: Maybe Expr
  }
  deriving (Eq, Ord, Show)

instance Semigroup Code where
  (Code a Nothing) <> (Code b ret) = Code (a <> b) ret
  _ <> _ = error "Can't prepend code with a return"

instance Monoid Code where
  mempty = Code [] Nothing

newtype ConcreteCode = ConcreteCode { unconcreteCode :: Code }
  deriving (Eq, Ord, Show)
  deriving Semigroup via Code
  deriving Monoid via Code

type Selection = Map SigName ImplName

showSelection :: Selection -> Text
showSelection = Text.intercalate "," . map (\(SigName sig, ImplName impl) -> sig <> ":" <> impl) . Map.toList

data ModularCode = ModularCode
  { moduleInstances :: Set (SigName, Maybe FieldName, [Expr]),
    modularCode :: Code
  }
  deriving (Eq, Ord, Show)

data Param = Param { unParam :: Text } deriving (Eq, Ord, Show)

newtype SigName = SigName { unSigName :: Symbol } deriving (Eq, Ord, Show)
-- Module system types
newtype FieldName = FieldName { unFieldName :: Symbol } deriving (Eq, Ord, Show)
-- SigName.FieldName
newtype FullSigName = FullSigName { unFullSigName :: Symbol } deriving (Eq, Ord, Show)

newtype ImplName = ImplName { unImplName :: Symbol } deriving (Eq, Ord, Show)
type ImplID = (Maybe SigName, ImplName)

-- Program types

-- Constraint:
-- for all `implementations`, the body follows the matching signature in `signatures`
data ModularProgram = ModularProgram
  { signatures :: Set (Type, SigName), -- Constraints: Unique SigName
    implementations :: Set (ModuleImplementation ModularCode),
    topFunctions :: Maybe ModularCode,
    topData :: [Text],
    topTD :: Maybe ModularCode,
    topParams :: Set Param,
    topBody :: ModularCode,
    topGQ :: Maybe ModularCode
  } deriving (Show)

data ModuleField code = ModuleField
  { fieldBody :: code
  , fieldArgs :: [Symbol]
  , fieldSignature :: Maybe FieldName
  }
  deriving (Eq, Ord, Show, Functor)

data ModuleImplementation code = ModuleImplementation
  { implName :: ImplName,
    implFields :: [ModuleField code],
    implSignature :: SigName,
    implFunctions :: Maybe code,
    implParams :: Set Param,
    implTD :: Maybe code,
    implGQ :: Maybe code
  }
  deriving (Eq, Ord, Show, Functor)

data ConcreteProgram = ConcreteProgram
  { concreteBody :: ConcreteCode,
    concreteFunctions :: ConcreteCode,
    concreteData :: [Text],
    concreteParams :: Set Param,
    concreteTD :: ConcreteCode,
    concreteGQ :: ConcreteCode
  } deriving Show
