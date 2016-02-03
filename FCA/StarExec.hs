module FCA.StarExec where

import Import

import Data.List
import Data.Text (append, pack)


type JobPairId = Int

data Attribute =
  AJobResultInfoSolver Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
 deriving (Eq, Ord, Show)

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
properName (AJobResultInfoSolver name) = append "Solver " name
properName (AJobResultInfoConfiguration config) = append "Solver config " config
properName (ASlowCpuTime False) = "CPU time <= 10s"
properName (ASlowCpuTime True) = "CPU time > 10s"
properName (ASolverResult YES) = "Result YES"
properName (ASolverResult NO) = "Result NO"
properName (ASolverResult MAYBE) = "Result MAYBE"
properName (ASolverResult (OTHER text)) = append "Result OTHER " text
properName (ASolverResult (BOUNDS b)) = append "Result BOUNDS " $ pack $ show b
properName (ASolverResult CERTIFIED) = "Result Certified"
properName (ASolverResult ERROR) = "Result Error"
