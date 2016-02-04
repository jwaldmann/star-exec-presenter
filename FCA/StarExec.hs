module FCA.StarExec where

import FCA.Utils
import Import
import Presenter.Model.Entities()
import Presenter.PersistHelper

import Data.List
import Data.Text (append, pack)


type JobPairId = Int

data Attribute =
  AJobResultInfoSolver Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
 deriving (Eq, Ord, Show)


-- get context of job results by given JobID
jobResultsContext:: JobID -> Handler (Context JobPairId Attribute)
jobResultsContext jid = do
  jobResults <- getPersistJobResults jid
  return $ contextFromList . collectData $ getStarExecResults jobResults

-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: (Num Double, Ord Double) => Double
slowCpuTimeLimit = 10

-- create relation of JobPairId and declared attributes of given data
collectData :: [JobResultInfo] -> [(JobPairId, [Attribute])]
collectData results = do
  let jobResultInfoPairIds = map jobResultInfoPairId results
  let attrs = getAttributeCollection results
  zip jobResultInfoPairIds attrs

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

-- proper name for template table columns
properName :: Attribute -> Text
properName at = case at of
 (AJobResultInfoSolver name)          -> append "Solver " name
 (AJobResultInfoConfiguration config) -> append "Solver config " config
 (ASlowCpuTime fast)    -> case fast of
                            False         -> "CPU time <= 10s"
                            True          -> "CPU time > 10s"
 (ASolverResult result) -> case result of
                            YES           -> "Result YES"
                            NO            -> "Result NO"
                            MAYBE         -> "Result MAYBE"
                            (BOUNDS b)    -> append "Result BOUNDS " $ pack $ show b
                            CERTIFIED     -> "Result Certified"
                            ERROR         -> "Result Error"
                            (OTHER text)  -> append "Result OTHER " text
