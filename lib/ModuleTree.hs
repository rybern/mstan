{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module ModuleTree (
    growTree
  , printModularTree
  , implSigs
  , findHighestModels
  , moduleTreeGraphviz
  , ModuleBranch (..)
  , Node (..)
  ) where



import           Data.List                ( groupBy, sortOn )
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text, intercalate)
import qualified Data.Text.IO             as Text
import           Data.Fix                 (refold)
import           Control.Applicative      (liftA2)
import           Graphviz
import           Types
import           Indent


------
-- Data structure manipulation for graph/tree abstractions
------

-- Mapping from each signature to all of its implementations
sigImpls :: ModularProgram -> Map SigName [ImplName]
sigImpls p = Map.fromList
        . map (\impls -> let sig = implSignature (head impls)
                         in  (sig, map implName impls))
        . groupBy (\x y -> implSignature x == implSignature y)
        . sortOn implSignature
        $ implementations p

-- Mapping from each implementation to all of its signatures
implSigs :: ModularProgram -> Map ImplID [SigName]
implSigs p = Map.insert Root (moduleSigs (topProgram p))
             . Map.fromList
             . map (\impl -> (ImplID (implSignature impl) (implName impl), moduleSigs impl))
            $ implementations p
  where
    codeSigs = Set.map (\(SigName sig, _, _) -> SigName sig) . moduleInstances
    moduleSigs :: Foldable t => t ModularCode -> [SigName]
    moduleSigs = orderSigs . Set.unions . concatMap (\code -> [codeSigs code])

    orderSigs :: Set SigName -> [SigName]
    orderSigs set = filter (`Set.member` set) . map snd $ signatures p


-- Pattern functor for module tree
data ModuleBranch f = SigBranch SigName [f] | ImplBranch Selection [f]
  deriving (Eq, Ord, Functor)

data Node = Impl ImplID | Sig SigName

-- Produce one level of tree given a node; for use in `refold`
growTree :: ModularProgram -> Node -> ModuleBranch Node
growTree p = growTree' (implSigs p) (sigImpls p)

growTree' :: Map ImplID [SigName] -> Map SigName [ImplName] -> Node -> ModuleBranch Node
growTree' iToS _ (Impl impl) =
  ImplBranch (idToSel impl) . map Sig . fromJust $ Map.lookup impl iToS
growTree' _ sToI (Sig sig) =
  SigBranch sig . map (Impl . ImplID sig) . fromJust $ Map.lookup sig sToI

idToSel :: ImplID -> Selection
idToSel Root              = Map.empty
idToSel (ImplID sig impl) = Map.singleton sig impl



getFullImplString :: ImplID -> Text
getFullImplString (ImplID parent name) = (unSigName parent) <> ":" <> (unImplName name)
getFullImplString Root = ""

-- for implementations: 
--  if terminal: return self
--  else: return merge(self, union(left_signature, right_signature))
hierTraverseNode :: Node -> Map ImplID [SigName] -> Map SigName [ImplName] -> [Text]
hierTraverseNode (Impl impl) impl2SigMap sig2ImplMap = do
    let signames = Map.lookup impl impl2SigMap
    case signames of
      Just signames -> if length signames == 1
                  then do
                    -- If there's only 1 signature underneath the implementation, nothing more needs to be done
                    map (\x -> Data.Text.intercalate "," [getFullImplString impl, x]) (hierTraverseNode (Sig $ head signames) impl2SigMap sig2ImplMap)
                  else 
                    (
                      if length signames == 2
                        then do
                          -- If there's 2 signatures underneath the implementation, you need to do an equivalent of a  graph join
                          -- Where you take the combinations of each signature's top nodes
                          let first_sig = (hierTraverseNode (Sig $ head signames) impl2SigMap sig2ImplMap); second_sig = (hierTraverseNode (Sig $ last signames) impl2SigMap sig2ImplMap) in 
                             map (\(x, y) -> Data.Text.intercalate "," [getFullImplString impl, x, y]) (liftA2 (,) first_sig second_sig)
                        else [getFullImplString impl]
                    )
      Nothing -> [getFullImplString impl]

-- for signatures: find implementations that don't have the value 'no'
hierTraverseNode (Sig sig) impl2SigMap sig2ImplMap = do
  let impls = Map.lookup sig sig2ImplMap
  case impls of
    Just impls -> concat $ map (\x -> if (unImplName x) /= "no" then hierTraverseNode (Impl ImplID {parent=sig, name=x}) impl2SigMap sig2ImplMap else []) impls
    Nothing -> []



-- entry point is implementation 'root'. From there, we check if the number of 
-- signatures are > 1, which is when the actual signature substrees start
hierTraverseInit :: Node -> Map ImplID [SigName] -> Map SigName [ImplName] -> [Text]
hierTraverseInit (Impl x) implToSigMap sigToImplMap = do
  let signames = Map.lookup x implToSigMap
  case signames of
    Just signames -> if length signames >= 2
      then do 
        concat (map (\m -> hierTraverseNode (Sig m) implToSigMap sigToImplMap) signames)
      else hierTraverseInit (Sig $ head $ signames) implToSigMap sigToImplMap
    Nothing -> []

-- signatures cannot branch off into more than 1
hierTraverseInit (Sig x) implToSigMap sigToImplMap = do
  let implnames = Map.lookup x sigToImplMap
  case implnames of 
    Just implnames -> hierTraverseInit (Impl ImplID {parent=x, name=head implnames}) implToSigMap sigToImplMap
    Nothing -> []

findHighestModels :: ModularProgram -> [Text]
findHighestModels m = hierTraverseInit (Impl Root) (implSigs m) (sigImpls m)

------
-- Visualizations
------

-- Combine a node's subtrees' text representations
joinTextLines :: ModuleBranch [Text] -> [Text]
joinTextLines (SigBranch (SigName sig) implLines) =
  "[" <> sig <> "]" : concatMap (indent 1) implLines
joinTextLines (ImplBranch sel sigLines) =
  selLine ++ concatMap (indent 1) sigLines
  where selLine = case Map.elems sel of
          [] -> ["(root)"]
          impls -> map (\(ImplName impl) -> "(" <> impl <> ")") impls

-- Build up a modular tree, fold it down to text lines, print it
printModularTree :: ModularProgram -> IO ()
printModularTree p = mapM_ Text.putStrLn $ refold joinTextLines (growTree p) (Impl Root)

-- Representation of a modular tree in Graphviz format for output
moduleTreeGraphviz :: ModularProgram -> Graphviz
moduleTreeGraphviz p = moduleGraphToDot
    (ModuleGraph allImpls allSigs (implSigs p) (sigImpls p))
  where
    allSigs  = map (\(_, SigName sig) -> SigName sig) (signatures p)
    allImpls = Root : map
        (\impl -> ImplID (implSignature impl) (implName impl))
        (implementations p)
