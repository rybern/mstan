{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module ToGraph where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Seq

import qualified Data.Text.Lazy                as LazyText
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Data.GraphViz.Types
import           Data.GraphViz.Types.Generalised
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Attributes
import           Data.GraphViz.Commands.IO hiding (runCommand)
import           System.Process
import           Types

printGraph :: DotGraph Text -> Text
printGraph = Text.pack . LazyText.unpack . printDotGraph

publishGraph :: FilePath -> Graphviz -> IO FilePath
publishGraph fp g = do
  writeDotFile dotFP g
  pid <- runCommand $ "dot " <> dotFP <> " -T svg -o " <> svgFP
  _ <- waitForProcess pid
  return svgFP
  where dotFP = fp <> ".dot"
        svgFP = fp <> ".svg"


data ModuleGraph = ModuleGraph [ImplID] (Set SigName) (Map ImplID (Set SigName)) (Map SigName (Set ImplName)) deriving (Eq, Ord, Show)

data ModelNode = ModelNode Text deriving (Eq, Ord, Show)
data DeltaModule = DeltaModule Text Text Text deriving (Eq, Ord, Show)

data ModelGraph = ModelGraph (Set ModelNode) [(ModelNode, ModelNode, DeltaModule)] deriving (Eq, Ord, Show)

implID :: ImplID -> Text
implID (Just (SigName s), ImplName i) = s <> ":" <> i
implID (Nothing, _) = "root"

implLabel :: ImplName -> Attribute
implLabel (ImplName i) = toLabel i

sigID :: SigName -> Text
sigID (SigName s) = s

sigLabel :: SigName -> Attribute
sigLabel (SigName s) = toLabel s

modelNodeNode :: ModelNode -> DotStatement Text
modelNodeNode (ModelNode m) = DN $ DotNode m [toLabel m]
sigNodeNode :: SigName -> DotStatement Text
sigNodeNode s = DN $ DotNode (sigID s) [sigLabel s, Shape BoxShape]
implNodeNode :: ImplID -> DotStatement Text
implNodeNode i = DN $ DotNode (implID i) [implLabel (snd i)]
deltaModuleEdge
  :: (ModelNode, ModelNode, DeltaModule) -> DotStatement Text
deltaModuleEdge (ModelNode m1, ModelNode m2, DeltaModule _ _ _) =
    DE $ DotEdge m1 m2 []
sigImplEdge :: (ImplID, SigName) -> DotStatement Text
sigImplEdge (i, s) = DE $ DotEdge (implID i) (sigID s) []
implSigEdge :: (SigName, ImplID) -> DotStatement Text
implSigEdge (s, i) = DE $ DotEdge (sigID s) (implID i) []

selectionImplIDs :: Selection -> Map SigName ImplID
selectionImplIDs = Map.mapWithKey (\s i -> (Just s, i))

implIDs :: Map SigName (Set ImplName) -> Map SigName (Set ImplID)
implIDs = Map.mapWithKey (\s impls -> Set.map (Just s,) impls)

moduleGraphToDot :: ModuleGraph -> DotGraph Text
moduleGraphToDot (ModuleGraph impls sigs toSigs toImpls) = DotGraph
    { directedGraph   = True
    , strictGraph     = True
    , graphID         = Nothing
    , graphStatements = mconcat
        [ Seq.fromList . map implNodeNode $ impls
        , Seq.fromList . Set.toList . Set.map sigNodeNode $ sigs
        , Seq.fromList . fmap implSigEdge . adjToList . implIDs $ toImpls
        , Seq.fromList . fmap sigImplEdge . adjToList $ toSigs
        -- , legend
        ]
    }
  where adjToList = concatMap (\(a, sb) -> map (\b -> (a, b)) (Set.toList sb)) . Map.toList
        -- legend =
        --   Seq.fromList [ DN $ DotNode "legend_impl" [toLabel ("[Implementation]" :: Text)]
        --                , DN $ DotNode "legend_sig" [toLabel ("[Signature]" :: Text), Shape BoxShape]]

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

type Graphviz = DotGraph Text
