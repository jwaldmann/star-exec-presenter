module FCA.StarExec where

import FCA.Basic hiding (concepts)
import FCA.Helpers
import Import
import Presenter.Model.Entities()
import Presenter.PersistHelper

import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List hiding (isPrefixOf, stripPrefix)
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T (append, isPrefixOf, pack, stripPrefix, take)

data Attribute =
  ASolverBasename Text
   | AJobResultInfoSolver Text
   | AYearSpecificSolverName Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
  deriving (Eq, Ord, Show)


-- get attribute pairs of given job ids
attributePairs :: [JobID] -> Handler [(JobPairID, [Attribute])]
attributePairs ids = do
  jobResults <- mapM getPersistJobResults ids
  competitionYears <- mapM getCompetitionYear ids
  return $ concatMap (\(jr, year) -> collectData (getStarExecResults jr) year) $ zip jobResults competitionYears

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
collectData :: [JobResultInfo] -> Text -> [(JobPairID, [Attribute])]
collectData results year = zip (map (StarExecPairID . jobResultInfoPairId) results) (getAttributeCollection results year)

-- create collection of selected attributes of given data
getAttributeCollection :: [JobResultInfo] -> Text -> [[Attribute]]
getAttributeCollection jobResults year = do
  let jobResultInfoSolvers = map jobResultInfoSolver jobResults
  let solverBasenames = map (getSolverBasename . jobResultInfoSolver) jobResults
  let yearSpecificSolverNames = map (`T.append` year) solverBasenames
  let jobResultInfoConfigurations = map
                                   (\(jr,name) -> name `append` (dashPrefix $ jobResultInfoConfiguration jr)) $
                                   zip jobResults yearSpecificSolverNames
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = map jobResultInfoResult jobResults
  zipWith6
    (\a b c d e f-> [
      AJobResultInfoSolver a,
      ASolverBasename b,
      AYearSpecificSolverName c,
      AJobResultInfoConfiguration d,
      ASlowCpuTime e,
      ASolverResult f
    ])
    jobResultInfoSolvers solverBasenames yearSpecificSolverNames jobResultInfoConfigurations cpuTimeEvaluations jobResultInfoResults

-- evaluate whether time are slow or not
evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map ((> slowCpuTimeLimit). jobResultInfoCpuTime)

-- proper names for attributes in template
properAttrName :: Attribute -> Text
properAttrName at = case at of
 (AJobResultInfoSolver name)          -> T.append "Solver name " name
 (ASolverBasename name)               -> T.append "Solver basename " name
 (AYearSpecificSolverName name)               -> T.append "SolverYearName " name
 (AJobResultInfoConfiguration config) -> T.append "Solver config " config
 (ASlowCpuTime slow)    -> if slow then "CPU time > 10s" else "CPU time <= 10s"
 (ASolverResult result) -> case result of
                            YES           -> "Result YES"
                            NO            -> "Result NO"
                            MAYBE         -> "Result MAYBE"
                            (BOUNDS b)    -> T.append "Result BOUNDS " $ T.pack $ show b
                            CERTIFIED     -> "Result CERTIFIED"
                            ERROR         -> "Result ERROR"
                            (OTHER text)  -> T.append "Result OTHER " text


stripAttributePrefixes :: Text -> Text
stripAttributePrefixes at
  | "Result " `T.isPrefixOf` at = fromJust $ T.stripPrefix "Result " at
  | "Solver config " `isPrefixOf` at = fromJust $ T.stripPrefix "Solver config " at
  | "Solver basename " `T.isPrefixOf` at = fromJust $ T.stripPrefix "Solver basename " at
  | "SolverYearName " `T.isPrefixOf` at = fromJust $ T.stripPrefix "SolverYearName " at
  | "Solver name " `T.isPrefixOf` at = fromJust $ T.stripPrefix "Solver name " at
  | "CPU " `T.isPrefixOf` at = fromJust $ T.stripPrefix "CPU " at
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

-- get competition year from JobID
getCompetitionYear :: JobID -> Handler Text
getCompetitionYear jid = do
  jobInfo <- getPersistStarExecJobInfo $ getStarExecId jid
  return $ dashPrefix $ T.take 4 . jobInfoDate $ fromJust jobInfo
