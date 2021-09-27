{-# LANGUAGE OverloadedStrings #-}
module ToGraph where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List
import qualified Data.Sequence                 as Seq

import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.IO             as LazyText
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           Data.GraphViz.Types
import           Data.GraphViz.Parsing
import           Data.GraphViz.Types.Generalised
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Attributes
import           Data.GraphViz.Commands.IO hiding (runCommand)
import           System.Process
import           Types

-- writeDotFile' = writeDotFile
printGraph :: DotGraph Text -> Text
printGraph = Text.pack . LazyText.unpack . printDotGraph

publishGraph :: FilePath -> Graph -> IO FilePath
publishGraph fp g = do
  writeDotFile dotFP g
  pid <- runCommand $ "dot " <> dotFP <> " -T svg -o " <> svgFP
  waitForProcess pid
  return svgFP
  where dotFP = fp <> ".dot"
        svgFP = fp <> ".svg"


data ModuleGraph = ModuleGraph (Set ImplNode) (Set SigNode) (Map ImplNode (Set SigNode)) (Map SigNode (Set ImplNode)) deriving (Eq, Ord, Show)

data ModelNode = ModelNode Text deriving (Eq, Ord, Show)
data DeltaModule = DeltaModule Text Text Text deriving (Eq, Ord, Show)

data ModelGraph = ModelGraph (Set ModelNode) [(ModelNode, ModelNode, DeltaModule)] deriving (Eq, Ord, Show)



implID :: ImplNode -> Text
implID (ImplNode (Just (SigNode s)) i) = s <> ":" <> i
implID (ImplNode Nothing i) = "root"

implLabel :: ImplNode -> Attribute
implLabel (ImplNode _ i) = toLabel i

sigID :: SigNode -> Text
sigID (SigNode s) = s

sigLabel :: SigNode -> Attribute
sigLabel (SigNode s) = toLabel s

modelNodeNode (ModelNode m) = DN $ DotNode m [toLabel m]
sigNodeNode s = DN $ DotNode (sigID s) [sigLabel s, Shape BoxShape]
implNodeNode i = DN $ DotNode (implID i) [implLabel i]
deltaModuleEdge (ModelNode m1, ModelNode m2, DeltaModule s i1 i2) =
    DE $ DotEdge m1 m2 [] --[toLabel $ s <> ": " <> i1 <> " <-> " <> i2]
sigImplEdge (i, s) = DE $ DotEdge (implID i) (sigID s) []
implSigEdge (s, i) = DE $ DotEdge (sigID s) (implID i) []

moduleGraphToDot :: ModuleGraph -> DotGraph Text
moduleGraphToDot (ModuleGraph impls sigs toSigs toImpls) = DotGraph
    { directedGraph   = True
    , strictGraph     = True
    , graphID         = Nothing
    , graphStatements = mconcat
        [ Seq.fromList . Set.toList . Set.map implNodeNode $ impls
        , Seq.fromList . Set.toList . Set.map sigNodeNode $ sigs
        , Seq.fromList . fmap implSigEdge . adjToList $ toImpls
        , Seq.fromList . fmap sigImplEdge . adjToList $ toSigs
        -- , legend
        ]
    }
  where adjToList = concatMap (\(a, sb) -> map (\b -> (a, b)) (Set.toList sb)) . Map.toList
        legend = 
          Seq.fromList [ DN $ DotNode "legend_impl" [toLabel ("[Implementation]" :: Text)]
                       , DN $ DotNode "legend_sig" [toLabel ("[Signature]" :: Text), Shape BoxShape]]

modelGraphToDot :: ModelGraph -> DotGraph Text
modelGraphToDot (ModelGraph nodes edges) = DotGraph
    { directedGraph   = False
    , strictGraph     = True
    , graphID         = Nothing
    , graphStatements = mconcat
                            [ Seq.fromList
                            . Set.toList
                            . Set.map modelNodeNode
                            $ nodes
                            , Seq.fromList . fmap deltaModuleEdge $ edges
                            ]
    }

type Graph = DotGraph Text
