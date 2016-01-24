module ConceptAnalysis.FCAPreparation where

import Import as I
import Data.List
import Data.Text.Lazy as TL hiding (map, zip)
import Presenter.Utils.Colors as C

type JobPairId = Int

data Attribute =
  AJobResultInfoSolver I.Text
   | AJobResultInfoConfiguration I.Text
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
properName :: Attribute -> TL.Text
properName (AJobResultInfoSolver name) = TL.append "Solver " $ fromStrict name
properName (AJobResultInfoConfiguration config) = TL.append "Solver config " $ fromStrict config
properName (ASlowCpuTime False) = "CPU time <= 10s"
properName (ASlowCpuTime True) = "CPU time > 10s"
properName (ASolverResult YES) = "Result YES"
properName (ASolverResult NO) = "Result NO"
properName (ASolverResult MAYBE) = "Result MAYBE"
properName (ASolverResult (OTHER text)) =  TL.append "Result OTHER " $ fromStrict text
properName (ASolverResult (BOUNDS b)) = append "Result " $ pack $ show b
properName (ASolverResult CERTIFIED) = "Result Certified"
properName (ASolverResult ERROR) = "Result Error"
