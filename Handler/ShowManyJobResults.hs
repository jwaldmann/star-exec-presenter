module Handler.ShowManyJobResults
  ( getShowManyJobResultsR
  , getShowManyJobResultsLegacyR
  ) where

import Import
import StarExec.Types
import StarExec.Persist
import StarExec.JobData
import Data.Double.Conversion.Text
import qualified Data.List as L
import qualified Data.Text as T
import Text.Lucius (luciusFile)
import Table.Query

countResults :: SolverResult -> (Int, (Int, Text)) -> Handler Int
countResults result (_jobId,(sid,_)) = runDB $ do
  count [ JobResultInfoJobId ==. _jobId
        , JobResultInfoSolverId ==. sid
        , JobResultInfoResult ==. result
        ]

countResultsYes :: (Int, (Int, Text)) -> Handler Int
countResultsYes (_jobId,(sid,_)) = runDB $ do
  count [ JobResultInfoJobId ==. _jobId
        , JobResultInfoSolverId ==. sid
        , JobResultInfoResult !=. NO
        , JobResultInfoResult !=. MAYBE
        , JobResultInfoResult !=. CERTIFIED
        , JobResultInfoResult !=. ERROR
        , JobResultInfoResult !=. OTHER
        ]

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

shorten :: Text -> Text
shorten t = if T.length t > 50
              then shorten $ T.tail t
              else t

--  | to keep old URLs working, as in
--  http://lists.lri.fr/pipermail/termtools/2014-July/000965.html
getShowManyJobResultsLegacyR :: JobIds -> Handler Html
getShowManyJobResultsLegacyR = getShowManyJobResultsR

getShowManyJobResultsR :: JobIds -> Handler Html
getShowManyJobResultsR jids @ (JobIds ids) = do
  qJobs <- queryManyJobResults ids
  let pJobs = map queryResult qJobs
      jobs = pJobs
      jobResults = concat jobs
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
      results = [ "YES", "NO", "MAYBE", "CERTIFIED", "ERROR", "OTHER" ] :: [T.Text]
  yesses <- mapM countResultsYes jobSolvers
  nos    <- mapM (countResults NO) jobSolvers
  maybes <- mapM (countResults MAYBE) jobSolvers
  certs  <- mapM (countResults CERTIFIED) jobSolvers
  errors <- mapM (countResults ERROR) jobSolvers
  others <- mapM (countResults OTHER) jobSolvers
  let scores = zip results [ yesses, nos, maybes, certs, errors, others ]
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    $(widgetFile "show_many_job_results")
