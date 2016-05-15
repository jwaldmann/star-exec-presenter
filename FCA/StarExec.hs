module FCA.StarExec where

import FCA.Utils hiding (concepts)
import Import
import Presenter.Model.Entities()
import Presenter.PersistHelper

import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List hiding (isPrefixOf, stripPrefix)
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (append, pack, isPrefixOf, stripPrefix)


data Attribute =
  AJobResultInfoSolver Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
  deriving (Eq, Ord, Show)


-- get attribute pairs of given job ids
attributePairs :: [JobID] -> Handler [(JobPairID, [Attribute])]
attributePairs ids = do
  jobResults <- mapM getPersistJobResults ids
  return $ concatMap (collectData . getStarExecResults) jobResults

  -- filter all job pairs by given attribute groups
filterPairs :: [(JobPairID, [Attribute])] -> [[Attribute]] -> Maybe [(JobPairID, [Attribute])]
filterPairs pairs chosenAts = do
  -- chosenAtsCombination contains all allowed attribute combinations
  let chosenAtsCombination = foldr (\a b -> (:) <$> a <*> b) [[]] chosenAts
  let anyMember = any id . (\s -> map (\v -> Set.isSubsetOf (Set.fromList v) s) chosenAtsCombination)
  let filteredJobResults = filter (\(_,ats) -> anyMember $ Set.fromList ats) pairs
  case filteredJobResults of
    [] -> Nothing
    _ -> Just filteredJobResults

-- unite all attributes of given jobpair attributes
uniteJobPairAttributes :: [(JobPairID, [Attribute])] -> Set Attribute
uniteJobPairAttributes pairs = Set.fromList $ concatMap snd pairs

-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: (Num Double, Ord Double) => Double
slowCpuTimeLimit = 10

-- create relation of JobPairID and declared attributes of given data
collectData :: [JobResultInfo] -> [(JobPairID, [Attribute])]
collectData results = zip (map (StarExecPairID . jobResultInfoPairId) results) (getAttributeCollection results)

-- create collection of selected attributes of given data
getAttributeCollection :: [JobResultInfo] -> [[Attribute]]
getAttributeCollection jobResults = do
  let jobResultInfoSolvers = map jobResultInfoSolver jobResults
  let jobResultInfoConfigurations = map jobResultInfoConfiguration jobResults
  -- let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = map jobResultInfoResult jobResults
  zipWith4 (\a b c d -> [AJobResultInfoSolver a, AJobResultInfoConfiguration b, ASlowCpuTime c, ASolverResult d])
    jobResultInfoSolvers jobResultInfoConfigurations cpuTimeEvaluations jobResultInfoResults

-- evaluate whether time are slow or not
evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map ((> slowCpuTimeLimit). jobResultInfoCpuTime)

-- proper names for attributes in template
properAttrName :: Attribute -> Text
properAttrName at = case at of
 (AJobResultInfoSolver name)          -> append "Solver name " name
 (AJobResultInfoConfiguration config) -> append "Solver config " config
 (ASlowCpuTime slow)    -> if slow then "CPU time > 10s" else "CPU time <= 10s"
 (ASolverResult result) -> case result of
                            YES           -> "Result YES"
                            NO            -> "Result NO"
                            MAYBE         -> "Result MAYBE"
                            (BOUNDS b)    -> append "Result BOUNDS " $ pack $ show b
                            CERTIFIED     -> "Result CERTIFIED"
                            ERROR         -> "Result ERROR"
                            (OTHER text)  -> append "Result OTHER " text


stripAttributePrefixes :: Text -> Text
stripAttributePrefixes at
  | "Result " `isPrefixOf` at = fromJust $ stripPrefix "Result " at
  | "Solver config " `isPrefixOf` at = fromJust $ stripPrefix "Solver config " at
  | "Solver name " `isPrefixOf` at = fromJust $ stripPrefix "Solver name " at
  | "CPU " `isPrefixOf` at = fromJust $ stripPrefix "CPU " at
  | otherwise = at


-- create all attribute combinations from existing attributes without duplicates
attributeCombination :: (Ord at) => Context ob at -> [Set at]
attributeCombination context = do
  let ats = Map.elems $ fore context
  -- using ordNub to reduce duplicate items and keep order
  ordNub $ map Set.fromList $ concatMap (subsequences . Set.toList) $ ordNub ats

-- determine all concepts of given context with StarExec attributes
concepts :: (Ord at, Ord ob, Show ob, Show at) => Context ob at -> [Concept ob at]
concepts c = do
  ats <- attributeCombination c
  guard $ ats == getAttributes c (getObjects c ats)
  return (Concept (getObjects c ats) ats)


-- https://github.com/nh2/haskell-ordnub#dont-use-nub
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
