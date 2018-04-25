module FCA.Basic where

import Import

import Control.Monad (guard)
import Data.List
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Begriff.Context as BX
import qualified Data.Begriff.Concept as BP
import qualified Data.Begriff.Build   as BB


import FCA.Helpers
-- example:
-- let c = contextFromList [(1,["foo", "bar"]), (2, ["foo","baz"])]
-- getAttributes c $ Set.fromList [1,2]
-- getObjects c $ Set.fromList ["foo"]
-- concepts c


data Context ob at = Context
  { fore :: Map ob (Set at)
  , back :: Map at (Set ob)
  } deriving (Show)

data Concept ob at = Concept
  { obs :: Set ob
  , ats :: Set at
  } deriving (Show, Eq)


-- determine all concepts of given context
concepts :: (Ord at, Ord ob) => Context ob at -> [Concept ob at]
concepts = concepts_bfs

lattice = BB.lattice
implications = BB.implications

einpack c = BX.build $ do
    (x, ys) <- contextToList c ; y <- ys ; return (x,y)

auspack node = Concept { obs = BP.objects node , ats = BP.attributes node }

concepts_bfs c = do
  (node, neighbours) <- BB.lattice $ einpack c
  return $ auspack node

concepts_basic c = do
  attrs <- (map Set.fromList . subsequences) . Set.toList $ attributes c
  guard $ attrs == getAttributes c (getObjects c attrs)
  return (Concept (getObjects c attrs) attrs)

-- get all attributes of given context
attributes :: (Ord at, Ord ob) => Context ob at -> Set at
attributes c = Map.keysSet $ back c

-- get all objects of given context
objects :: (Ord at, Ord ob) => Context ob at -> Set ob
objects c = Map.keysSet $ fore c

-- get all attributes of given context and specific objects
getAttributes :: (Ord at, Ord ob) => Context ob at -> Set ob -> Set at
getAttributes c objs = foldr Set.intersection (attributes c)
  $ map (\o -> fore c Map.! o) $ Set.toList objs

-- get all objects of given context and specific attributes
getObjects :: (Ord at, Ord ob) => Context ob at -> Set at -> Set ob
getObjects c attrs = foldr Set.intersection (objects c)
  $ map (\a -> back c Map.! a) $ Set.toList attrs

-- reduce concepts to concepts with proper subsets of given concept id
reduceConceptsToProperSubsets :: (Ord at) => Maybe [Concept ob at] -> ConceptId -> Maybe [Concept ob at]
reduceConceptsToProperSubsets conceptLattice cid = case conceptLattice of
                                    Nothing -> Nothing
                                    Just concepts' -> do
                                      let concept = safeGetIndex concepts' cid
                                      case concept of
                                        Nothing -> Nothing
                                        Just c  -> return $ c:filter (Set.isProperSubsetOf (ats c) . ats) concepts'

-- create a context of given input data
contextFromList :: (Ord at, Ord ob) => [(ob, [at])] -> Context ob at
contextFromList l = Context
  { fore=Map.fromListWith Set.union $ map (\(ob, attrs) -> (ob, Set.fromList attrs)) l
  , back=Map.fromListWith Set.union $ do (ob, attrs) <- l; at <- attrs; return (at,Set.singleton ob)
  }

-- create list of pairs of a context
contextToList :: (Ord ob) => Context ob at -> [(ob, [at])]
contextToList context = do
  let obAtsRel = fore context
  map (\k -> (k, Set.toList . fromJust $ Map.lookup k obAtsRel)) $ Map.keys obAtsRel
