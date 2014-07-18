module Handler.ShowManyJobResults
  ( getShowManyJobResultsR
  , getShowManyJobResultsLegacyR
  ) where

import Import
import StarExec.Types
import StarExec.JobData
import StarExec.Processing
import Data.Double.Conversion.Text
import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Text.Lucius (luciusFile)
import Table.Query
import Utils.WidgetMetaRefresh
import StarExec.Statistics

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

shorten :: Text -> Text
shorten t = if T.length t > 50
              then shorten $ T.tail t
              else t

calcScore :: [Int] -> Int -> Handler Int
calcScore [] _solverId = return 0
calcScore _jobIds _solverId = do
  let solverFilter = JobResultInfoSolverId ==. _solverId
      eqFilters = map (\i -> [JobResultInfoJobId ==. i, solverFilter]) _jobIds
      jobFilter = L.foldr1 (||.) $ eqFilters
  solverResults <- runDB $ selectList jobFilter []
  return $ sum $ catMaybes $ map (jobResultInfoScore . entityVal) solverResults

--  | to keep old URLs working, as in
--  http://lists.lri.fr/pipermail/termtools/2014-July/000965.html
getShowManyJobResultsLegacyR :: JobIds -> Handler Html
getShowManyJobResultsLegacyR = getShowManyJobResultsR

getShowManyJobResultsR :: JobIds -> Handler Html
getShowManyJobResultsR jids @ (JobIds ids) = do
  qJobs <- queryManyJobs ids
  let jobInfos = catMaybes $ map (fst . queryResult) qJobs
      isComplexity = all jobInfoIsComplexity jobInfos
      jobs = map (snd . queryResult) qJobs

      jobResults :: [JobResultInfo]
      jobResults = concat $ jobs

      stat = Statistics 
           { complete = False -- don't know
           , startTime = Nothing, finishTime = Nothing
           , cpu = sum $ map jobResultInfoCpuTime jobResults
           , wallclock = sum $ map jobResultInfoWallclockTime jobResults
           , pairs = length jobResults
           , pairsCompleted = length $ filter ( ( == JobResultComplete) . jobResultInfoStatus ) jobResults
           }

      benchmarks = L.sortBy compareBenchmarks $
                    getInfo extractBenchmark $ jobResults
      groupedSolvers = map (getInfo extractSolver) jobs
      jobSolvers = concat $ map toTuples $ zip ids groupedSolvers
      solvers = concat $ groupedSolvers
      benchmarkResults = getBenchmarkResults
                          solvers
                          jobResults
                          benchmarks
      (+>) = T.append
  let scores = flip map jobs $
        \results ->
          if isComplexity
            then calcComplexityScores results
            else calcStandardScores results
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q -> queryStatus q /= Latest) qJobs
      then insertWidgetMetaRefresh
      else return ()
    $(widgetFile "show_many_job_results")
