module Handler.ShowJobResults where

import Import
import StarExec.Types
import StarExec.Persist
import StarExec.JobResultsTable
import Data.Double.Conversion.Text
import qualified Data.List as L

countResults _jobId result solverName = do
  count [ PersistJobResultInfoStarExecJobId ==. _jobId
        , PersistJobResultInfoSolver ==. solverName
        , PersistJobResultInfoResult ==. result ]

getShowJobResultsR :: Int -> Handler Html
getShowJobResultsR _jobId = do
  pJobInfos <- getJobResults _jobId
  let jobinfos = fromPersistJobResultInfos pJobInfos
      benchmarks = getInfo extractBenchmark jobinfos
      solvers = getInfo extractSolver jobinfos
      benchmarkResults = getBenchmarkResults
                          solvers
                          jobinfos
                          (L.sortBy compareBenchmarks benchmarks)
      solverNames = map snd solvers
      countResultsByJobId = countResults _jobId
      results = [ YES, NO, MAYBE, CERTIFIED, ERROR, OTHER ]
  yesses <- runDB $ mapM (countResultsByJobId YES) solverNames
  nos    <- runDB $ mapM (countResultsByJobId NO) solverNames
  maybes <- runDB $ mapM (countResultsByJobId MAYBE) solverNames
  certs  <- runDB $ mapM (countResultsByJobId CERTIFIED) solverNames
  errors <- runDB $ mapM (countResultsByJobId ERROR) solverNames
  others <- runDB $ mapM (countResultsByJobId OTHER) solverNames
  let scores = zip results [ yesses, nos, maybes, certs, errors, others ]
  defaultLayout $ do
    $(widgetFile "show_job_results")
