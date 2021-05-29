{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

type Symbol = Text

-- Stan-ish types
newtype Type = Type Text deriving (Eq, Ord, Show)

newtype Expr = Expr {unExpr :: Text} deriving (Eq, Ord, Show)

data Code = Code
  { codeText :: [Text],
    codeReturn :: Maybe Expr
  }
  deriving (Eq, Ord, Show)

data ConcreteCode = ConcreteCode { unconcreteCode :: Code } deriving (Eq, Ord, Show)

data ModularCode = ModularCode
  { moduleInstances :: Set (SigName, Maybe InstanceName, [Expr]),
    modularCode :: Code
  }
  deriving (Eq, Ord, Show)

data Param = Param Text deriving (Eq, Ord, Show)

-- Module system types
type SigName = Symbol

type ImplName = Symbol

type InstanceName = Symbol

-- Program types

-- Constraint:
-- for all `implementations`, the body follows the matching signature in `signatures`
data ModularProgram = ModularProgram
  { signatures :: Set (Type, SigName), -- Constraints: Unique SigName
    implementations :: Set (ModuleImplementation ModularCode),
    topBody :: ModularCode,
    topData :: [Text],
    topParams :: Set Param
  } deriving (Show)

data ModuleImplementation code = ModuleImplementation
  { implBody :: code,
    implArgs :: [Symbol],
    implSignature :: SigName,
    implParams :: Set Param,
    implName :: ImplName
  }
  deriving (Eq, Ord, Show)

data ConcreteProgram = ConcreteProgram
  { concreteBody :: ConcreteCode,
    concreteData :: [Text],
    concreteParams :: Set Param
  } deriving Show

printConcreteProgram :: ConcreteProgram -> IO ()
printConcreteProgram = mapM_ Text.putStrLn . linesConcreteProgram

indent :: Int -> [Text] -> [Text]
indent n = map (Text.replicate n "  " <>)

linesConcreteProgram :: ConcreteProgram -> [Text]
linesConcreteProgram p = ["parameters {"]
        ++ map (\(Param p) -> "  " <> p <> ";") (Set.toList (concreteParams p))
        ++ [ "}"
           , "data {"]
        ++ indent 1 (concreteData p)
        ++ [ "}"
           , "model {"]
        ++ indent 1 (codeText (unconcreteCode (concreteBody p)))
        ++ [ "}" ]

