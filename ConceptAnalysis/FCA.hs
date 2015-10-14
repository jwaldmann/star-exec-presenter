module ConceptAnalysis.FCA where

import Import
import Control.Monad (guard)
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- example: let c = contextFromList [(1,["foo", "bar"]), (2, ["foo","baz"])]
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

-- create a context of given input data
contextFromList :: (Ord ob, Ord at) => [(ob, [at])] -> Context ob at
contextFromList l = Context 
  { fore=Map.fromListWith Set.union $ map (\(ob, ats) -> (ob, Set.fromList ats)) l
  , back=Map.fromListWith Set.union $ do (ob, ats) <- l; at <- ats; return (at,Set.singleton ob)
  }

-- get all attributes of given context
attributes :: (Ord ob, Ord at) => Context ob at -> Set at
attributes c = Map.keysSet $ back c

-- get all attributes of given context and specific objects
getAttributes :: (Ord ob, Ord at) => Context ob at -> Set ob -> Set at
getAttributes c obs = foldr Set.intersection (attributes c)
  $ map (\o -> fore c Map.! o) $ Set.toList obs

-- get all objects of given context
objects :: (Ord ob, Ord at) => Context ob at -> Set ob
objects c = Map.keysSet $ fore c

-- get all objects of given context and specific attributes
getObjects :: (Ord ob, Ord at) => Context ob at -> Set at -> Set ob
getObjects c ats = foldr Set.intersection (objects c)
  $ map (\a -> back c Map.! a) $ Set.toList ats

-- determine all concepts of given context
concepts :: (Ord ob, Ord at) => Context ob at -> [Concept ob at]
concepts c = do
  ats <- map (\ats -> Set.fromList ats) $ subsequences $ Set.toList $ attributes c
  guard $ ats == (getAttributes c $ getObjects c ats)
  return (Concept (getObjects c ats) ats)
