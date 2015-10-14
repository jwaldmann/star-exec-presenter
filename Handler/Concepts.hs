module Handler.Concepts where

import Import
import Data.Maybe
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
-- import Data.Text (append)
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA


type JobPairId = Int

data Attribute = 
  AJobResultInfoSolver Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
 deriving (Eq, Ord, Show)

-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: ((Num Double, Ord Double)) => Double
slowCpuTimeLimit = 10

-- route to show concepts of given JobID
getConceptsR :: JobID -> Handler Html
getConceptsR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  defaultLayout $ do
    setTitle "concepts"
    $(widgetFile "concepts")

-- create relation of JobPairId and declared attributes of given data
collectData :: [JobResultInfo] -> [(JobPairId, [Attribute])]
collectData results = do
  let jobResultInfoPairIds = map jobResultInfoPairId results
  let attrs = getAttributeCollection results
  zip jobResultInfoPairIds attrs

-- create collection of selected attributes of given data
getAttributeCollection :: [JobResultInfo] -> [[Attribute]]
getAttributeCollection jobResults = do
  let jobResultInfoSolvers = map (jobResultInfoSolver) jobResults
  let jobResultInfoConfigurations = map jobResultInfoConfiguration jobResults
  -- let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = map (jobResultInfoResult) jobResults
  zipWith4 (\a b c d -> [AJobResultInfoSolver a, AJobResultInfoConfiguration b, ASlowCpuTime c, ASolverResult d])
    jobResultInfoSolvers jobResultInfoConfigurations cpuTimeEvaluations jobResultInfoResults

-- evaluate whether time are slow or not
evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map ((> slowCpuTimeLimit). jobResultInfoCpuTime)

-- proper name for template table columns
-- properName :: Attribute -> Text
-- properName (AJobResultInfoSolver name) = append "solver name " name
-- properName (AJobResultInfoConfiguration config) = append "solver configuration " config
-- properName (ASlowCpuTime False) = "cpuTime <= 10s"
-- properName (ASlowCpuTime True) = "cpuTime > 10s"
-- properName (ASolverResult YES) = "result YES"
-- properName (ASolverResult NO) = "result NO"
-- properName (ASolverResult MAYBE) = "result MAYBE"
-- properName (ASolverResult (OTHER text)) = append "result OTHER" text
