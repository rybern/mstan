{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module SemanticChecking where

import Types

import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           qualified Data.Set as                        Set 
import           Data.Monoid
import           qualified Data.List as List
import qualified Data.Text                     as Text
-- import qualified Data.Graph               as Graph
-- import qualified Algebra.Graph.AdjacencyIntMap.Algorithm as Graph
-- import qualified Data.Foldable as Foldable

data SemanticError = ImplCycle [ImplID]
  deriving Show

showSemanticError :: SemanticError -> [String]
showSemanticError (ImplCycle c) = [ "There was a dependency cycle among the program's modules:"
                                  , List.intercalate (" -> ") (map (\(ImplID sigName implName) -> Text.unpack (unSigName sigName) ++ ":" ++ Text.unpack (unImplName implName)) c) 
                                  ]
                                  
semanticCheck :: ModularProgram -> [SemanticError]
semanticCheck p = maybeToList $ ImplCycle <$> findHoleCycle p

-- constraints: 
-- for all `implementations`, the body follows the matching signature in `signatures`
-- no cycles
-- no same sig names
-- no same impl and sig names
type I2H = Map ImplID (Set SigName)
type H2I = Map SigName (Set ImplID)
implSigs :: ModularProgram -> I2H 
implSigs p =
  Map.fromList . (root:) . flip map (implementations p) $ \impl ->
    (ImplID (implSignature impl) (implName impl), holes impl)
  where holes :: (Foldable t) => t ModularCode -> Set SigName
        holes = Set.map (\(sigName, _, _) -> sigName) . foldMap moduleInstances
        root = (Root, holes (topProgram p))

sigImpls :: ModularProgram -> H2I 
sigImpls (ModularProgram {..}) =
  Map.fromList . flip map signatures $ \(_, sigName) ->
    (sigName, Set.fromList $ map (ImplID sigName . implName) $ filter (\impl -> implSignature impl == sigName) implementations)

findHoleCycle :: ModularProgram -> Maybe [ImplID]
findHoleCycle p = findCycle Root i2i
  where i2i :: Map ImplID (Set ImplID)
        i2i = Map.mapWithKey (\i hs -> Set.unions $ Set.map (\h -> fromJust $ Map.lookup h h2i) hs) i2h
        i2h = implSigs p
        h2i = sigImpls p

findCycle :: (Ord a) => a -> Map a (Set a) -> Maybe [a]
findCycle root m = findCycle' [root] (Set.singleton root) m

test1 = Map.fromList . map (\(x:y) -> (x, Set.fromList y)) $ ["abc", "bcd", "cef", "dc", "e", "fa"]

c1 = findCycle 'a' test1

findCycle' :: (Ord a) => [a] -> Set a -> Map a (Set a) -> Maybe [a]
findCycle' [] _ _ = Nothing
findCycle' (root:path) seen m =
  let next = fromJust $ Map.lookup root m
      overlap = Set.intersection seen next
  in
  if not (Set.null overlap) then
    let found = Set.elemAt 0 overlap
    in Just $ dropWhile (/= found) (reverse (found:root:path))
  else
    unFirst . mconcat . map (\v -> First $ findCycle' (v:root:path) (Set.insert v seen) m) . Set.toList $ next

unFirst (First f) = f
