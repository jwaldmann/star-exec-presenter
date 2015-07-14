module Handler.DbTest where

import Import
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import           Data.Set (Set)
-- import qualified Data.Set as Set
import Presenter.PersistHelper
import Presenter.Model.Entities()


-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: ((Num Double, Ord Double)) => Double
slowCpuTimeLimit = 10.0

getDbTestR :: JobID -> Handler Html
getDbTestR jid = do
  jobResults <- getPersistJobResults jid
  let jobResultInfos = createObjectAttributeRelations $ getStarExecResults jobResults
  let x = Map.toList jobResultInfos
  let xn = length $ x
  defaultLayout [whamlet|
    #{show xn}
    <ul>
    $forall (jobPairId, jobPairAttributes) <- Map.toList jobResultInfos
      <li> #{show jobPairId}: #{show jobPairAttributes}
    |]
    -- <ul>
    -- $forall jobResult <- getStarExecResults jobResults
    --   <li> #{show jobResult}


createConcept :: Map Int (Int, Text, Bool, SolverResult) -> [(a,b)]
createConcept = undefined


createObjectAttributeRelations :: [JobResultInfo] -> Map Int (Int, Text, Bool, SolverResult)
createObjectAttributeRelations jobResults = do
  let jobResultInfoPairIds = map (jobResultInfoPairId) jobResults
  let attrs = getAttributeCollection jobResults
  let objAttrRel = zip jobResultInfoPairIds attrs
  Map.fromList objAttrRel


getAttributeCollection :: [JobResultInfo] -> [(Int, Text, Bool, SolverResult)]
getAttributeCollection jobResults = do
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoSolvers = map (jobResultInfoSolver) jobResults
  let jobResultInfoResults = map (jobResultInfoResult) jobResults
  let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  zip4 jobResultInfoBenchmarkIds jobResultInfoSolvers cpuTimeEvaluations jobResultInfoResults


evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map (isLow . jobResultInfoCpuTime)

isLow :: (Num Double, Ord Double) => Double -> Bool
isLow a = a > slowCpuTimeLimit

-- evaluateResult :: [JobResultInfo] -> [SolverResult]
-- evaluateResult = map (jobResultInfoResult)

-- copied from: http://rosettacode.org/wiki/Power_set#Haskell
powerset :: Foldable t => t a -> [[a]]
powerset = foldr (\x acc -> acc ++ map (x:) acc) [[]]



