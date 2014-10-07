module Handler.ShowManyJobResults where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Processing
import Presenter.Statistics
import Presenter.Utils.WidgetMetaRefresh
import Text.Lucius (luciusFile)
import Data.Double.Conversion.Text
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

shorten :: Text -> Text
shorten t = if T.length t > 50
              then shorten $ T.tail t
              else t

getShowManyJobResultsR :: Query -> JobIds -> Handler Html
getShowManyJobResultsR NoQuery  jids@(JobIds ids) = do
  qJobs <- queryManyJobs ids
  let jobInfos = catMaybes $ map (fst . queryResult) qJobs
      complexity = all isComplexity jobInfos
      jobs = map (snd . queryResult) qJobs

      jobResults :: [JobResult]
      jobResults = concat $ jobs

      stat = mconcat $ map jobStat jobResults

      benchmarks = L.sortBy compareBenchmarks $
                    getInfo extractBenchmark $ jobResults
      groupedSolvers = map (getInfo extractSolver) jobs
      jobSolvers = concat $ map toTuples $ zip ids groupedSolvers
      benchmarkResults = getBenchmarkResults
                          jobSolvers
                          jobResults
                          benchmarks
      (+>) = T.append
      scores = flip map jobs $
        \results ->
          if complexity
            then calcComplexityScores results
            else calcStandardScores results
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    if any (\q -> queryStatus q /= Latest) qJobs
      then insertWidgetMetaRefresh
      else return ()
    $(widgetFile "show_many_job_results")

getShowManyJobResultsR query ids = error "Not yet implemented: getShowManyJobResultsR with query"
