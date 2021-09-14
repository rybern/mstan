{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
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

instance Semigroup Code where
  (Code a Nothing) <> (Code b ret) = Code (a <> b) ret
  _ <> _ = error "Can't prepend code with a return"

newtype ConcreteCode = ConcreteCode { unconcreteCode :: Code }
  deriving (Eq, Ord, Show)
  deriving Semigroup via Code


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
    topGQ :: ModularCode,
    topData :: [Text],
    topParams :: Set Param
  } deriving (Show)

data ModuleImplementation code = ModuleImplementation
  { implBody :: code,
    implArgs :: [Symbol],
    implSignature :: SigName,
    implParams :: Set Param,
    implGQ :: Maybe code,
    implName :: ImplName
  }
  deriving (Eq, Ord, Show, Functor)

data ConcreteProgram = ConcreteProgram
  { concreteBody :: ConcreteCode,
    concreteData :: [Text],
    concreteParams :: Set Param,
    concreteGQ :: ConcreteCode
  } deriving Show

printConcreteProgram :: ConcreteProgram -> IO ()
printConcreteProgram = mapM_ Text.putStrLn . linesConcreteProgram

indentation :: Int -> Text
indentation n = Text.replicate n "  "

indent :: Int -> [Text] -> [Text]
indent n = map (indentation n <>)

-- Remove n leading spaces if all of the code has at least that many leading spaces
unindentCodeText :: Int -> [Text] -> [Text]
unindentCodeText n codeText = fromMaybe codeText $ mapM unindentCodeStmt codeText
  where unindentCodeStmt = ((Text.intercalate "\n" <$>) . unindentLines n . Text.lines)
        unindentLines :: Int -> [Text] -> Maybe [Text]
        unindentLines n (l:ls) = (l:) <$> mapM (Text.stripPrefix indent) ls
          where indent = indentation n

linesConcreteProgram :: ConcreteProgram -> [Text]
linesConcreteProgram p = concat
  [ [ "data {"]
  , indent 1 (concreteData p)
  , [ "}"]
  , ["parameters {"]
  , map (\(Param p) -> "  " <> p <> ";") (Set.toList (concreteParams p))
  , [ "}" ]
  , [ "model {"]
  , indent 1 (codeText (unconcreteCode (concreteBody p)))
  , [ "}" ]
  , [ "generated quantities {"]
  , indent 1 (unindentCodeText 1 (codeText (unconcreteCode (concreteGQ p))))
  , [ "}" ]
  ]

