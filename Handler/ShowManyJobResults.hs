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

countResults result (_jobId,(sid,_)) = do
  count [ PersistJobResultInfoStarExecJobId ==. _jobId
        , PersistJobResultInfoSolverId ==. sid
        , PersistJobResultInfoResult ==. result ]

toTuples :: (a, [b]) -> [(a,b)]
toTuples (i, solvers) = map ((,) i) solvers

getShowManyJobResultsR :: JobIds -> Handler Html
getShowManyJobResultsR (JobIds ids) = do
  pJobs <- getManyJobResults ids
  let jobs = map fromPersistJobResultInfos pJobs
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
  yesses <- runDB $ mapM (countResults YES) jobSolvers
  nos    <- runDB $ mapM (countResults NO) jobSolvers
  maybes <- runDB $ mapM (countResults MAYBE) jobSolvers
  certs  <- runDB $ mapM (countResults CERTIFIED) jobSolvers
  errors <- runDB $ mapM (countResults ERROR) jobSolvers
  others <- runDB $ mapM (countResults OTHER) jobSolvers
  let scores = zip results [ yesses, nos, maybes, certs, errors, others ]
  defaultLayout $ do
    $(widgetFile "show_many_job_results")
