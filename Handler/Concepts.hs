module Handler.Concepts where

import Import
import Data.Maybe
import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Presenter.PersistHelper
import Presenter.Model.Entities()
import ConceptAnalysis.FCA


type JobPairId = Int

data Attribute = 
  AJobResultInfoSolver Text
   | AJobResultInfoConfiguration Text
   | ASlowCpuTime Bool
   | ASolverResult SolverResult
   | ASlowCpuTimeSolverResult Bool SolverResult
 deriving (Eq, Ord, Show)

-- all job pairs with a response time greater 10 seconds is slow
slowCpuTimeLimit :: ((Num Double, Ord Double)) => Double
slowCpuTimeLimit = 10

getConceptsR :: JobID -> Handler Html
getConceptsR jid = do
  jobResults <- getPersistJobResults jid
  let contextData = collectData $ getStarExecResults jobResults
  let context = contextFromList contextData
  let concepts' = concepts context
  defaultLayout [whamlet|
    <table class="table table-bordered table-striped">
      <thead>
        <th> concepts
        $forall at <- attributes context
          <th> #{show at}
        <th> count
      <tbody>

      $forall (_,currAts) <- concepts'
        <tr>
          <td> Z
          $forall at <- attributes context
            $if elem at currAts
              <td>X
            $else
              <td>
          <td> #{length currAts}

    <h1>Concepts
    <ul>
    $forall (o,a) <- concepts'
      <li> #{show o} : #{show a}

    <h1>Objects
    <ul>
    $forall k <- objects context
      <li> #{show k}
      #{show $ getAttributes context $ Set.fromList [k]}

    <h1>Attributes
    <ul>
    $forall k <- attributes context
      <li> #{show k}
      #{show $ getObjects context $ Set.fromList [k]}

    <h1>ContextData
    <ul>
    $forall obj <- contextData
      <li> #{show obj}

    <h1>Original records
    <ul>
    $forall jobResult <- getStarExecResults jobResults
      <li> #{show jobResult}
    |]

collectData :: [JobResultInfo] -> [(JobPairId, [Attribute])]
collectData results = do
  let jobResultInfoPairIds = map jobResultInfoPairId results
  let attrs = getAttributeCollection results
  zip jobResultInfoPairIds attrs

getAttributeCollection :: [JobResultInfo] -> [[Attribute]]
getAttributeCollection jobResults = do
  let jobResultInfoSolvers = map (jobResultInfoSolver) jobResults
  let jobResultInfoConfigurations = map jobResultInfoConfiguration jobResults
  -- let jobResultInfoBenchmarkIds = map (jobResultInfoBenchmarkId) jobResults
  let cpuTimeEvaluations = evaluateCpuTime jobResults
  let jobResultInfoResults = map (jobResultInfoResult) jobResults
  zipWith4 (\a b c d -> [AJobResultInfoSolver a, AJobResultInfoConfiguration b, ASlowCpuTime c, ASolverResult d])
    jobResultInfoSolvers jobResultInfoConfigurations cpuTimeEvaluations jobResultInfoResults

evaluateCpuTime :: [JobResultInfo] -> [Bool]
evaluateCpuTime = map ((> slowCpuTimeLimit). jobResultInfoCpuTime)
