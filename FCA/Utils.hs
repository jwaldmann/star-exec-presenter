module FCA.Utils where

import Import

import Control.Monad (guard)
import Data.List
import Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

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


-- create a context of given input data
contextFromList :: (Ord at, Ord ob) => [(ob, [at])] -> Context ob at
contextFromList l = Context
  { fore=Map.fromListWith Set.union $ map (\(ob, ats) -> (ob, Set.fromList ats)) l
  , back=Map.fromListWith Set.union $ do (ob, ats) <- l; at <- ats; return (at,Set.singleton ob)
  }

-- create list of pairs of a context
contextToList :: (Ord ob) => Context ob at -> [(ob, [at])]
contextToList context = do
  let obAtsRel = fore context
  map (\k -> (k, Set.toList $ fromJust $ Map.lookup k obAtsRel)) $ Map.keys obAtsRel

-- reduce attributes of existing context and return reduced one
filterContext :: (Ord ats, Ord ob) => Context ob ats -> Set ats -> Context ob ats
filterContext context attrs = do
  let l = map (\(ob, ats) -> (ob, Set.toList $ Set.intersection attrs $ Set.fromList ats)) $ contextToList context
  contextFromList l

-- determine all concepts of given context
concepts :: (Ord at, Ord ob) => Context ob at -> [Concept ob at]
concepts c = do
  ats <- map Set.fromList $ subsequences $ Set.toList $ attributes c
  guard $ ats == getAttributes c (getObjects c ats)
  return (Concept (getObjects c ats) ats)

-- get all attributes of given context
attributes :: (Ord at, Ord ob) => Context ob at -> Set at
attributes c = Map.keysSet $ back c

-- get all objects of given context
objects :: (Ord at, Ord ob) => Context ob at -> Set ob
objects c = Map.keysSet $ fore c

-- get all attributes of given context and specific objects
getAttributes :: (Ord at, Ord ob) => Context ob at -> Set ob -> Set at
getAttributes c obs = foldr Set.intersection (attributes c)
  $ map (\o -> fore c Map.! o) $ Set.toList obs

-- get all objects of given context and specific attributes
getObjects :: (Ord at, Ord ob) => Context ob at -> Set at -> Set ob
getObjects c ats = foldr Set.intersection (objects c)
  $ map (\a -> back c Map.! a) $ Set.toList ats
