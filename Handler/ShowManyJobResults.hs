module Handler.ShowManyJobResults
  ( getShowManyJobResultsR
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
        , JobResultInfoResult ==. result ]

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

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
      solverNames = map (\(i,name) -> name +> " (" +> (T.pack $ show i) +> ")" ) solvers
      results = [ YES, NO, MAYBE, CERTIFIED, ERROR, OTHER ]
  yesses <- mapM (countResults YES) jobSolvers
  nos    <- mapM (countResults NO) jobSolvers
  maybes <- mapM (countResults MAYBE) jobSolvers
  certs  <- mapM (countResults CERTIFIED) jobSolvers
  errors <- mapM (countResults ERROR) jobSolvers
  others <- mapM (countResults OTHER) jobSolvers
  let scores = zip results [ yesses, nos, maybes, certs, errors, others ]
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    $(widgetFile "show_many_job_results")
