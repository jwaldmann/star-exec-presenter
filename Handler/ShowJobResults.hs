module Handler.ShowJobResults
  ( getShowJobResultsR
  ) where

import Import
import StarExec.Types
import StarExec.Persist
import StarExec.PersistTypes
import StarExec.JobData
import Data.Double.Conversion.Text
import qualified Data.List as L
import Text.Lucius (luciusFile)

countResults :: Int -> SolverResult -> Text -> Handler Int
countResults _jobId result _solverName = runDB $ do
  count [ JobResultInfoJobId ==. _jobId
        , JobResultInfoSolver ==. _solverName
        , JobResultInfoResult ==. result ]

checkStatus :: QueryStatus QueryInfo -> Handler ()
checkStatus Latest = liftIO $ putStrLn "Job Results up-to-date..."
checkStatus (Pending key) = liftIO $ putStrLn $ "Job pending: " ++ (show key)

getShowJobResultsR :: Int -> Handler Html
getShowJobResultsR _jobId = do
  (QueryResult qStatus jrs) <- queryJobResults _jobId
  checkStatus qStatus
  --pJobResults <- getJobResults _jobId
  let jobResults = jrs
      benchmarks = getInfo extractBenchmark jobResults
      solvers = getInfo extractSolver jobResults
      benchmarkResults = getBenchmarkResults
                          solvers
                          jobResults
                          (L.sortBy compareBenchmarks benchmarks)
      solverNames = map snd solvers
      countResultsByJobId = countResults _jobId
      results = [ YES, NO, MAYBE, CERTIFIED, ERROR, OTHER ]
  yesses <- mapM (countResultsByJobId YES) solverNames
  nos    <- mapM (countResultsByJobId NO) solverNames
  maybes <- mapM (countResultsByJobId MAYBE) solverNames
  certs  <- mapM (countResultsByJobId CERTIFIED) solverNames
  errors <- mapM (countResultsByJobId ERROR) solverNames
  others <- mapM (countResultsByJobId OTHER) solverNames
  let scores = zip results [ yesses, nos, maybes, certs, errors, others ]
  defaultLayout $ do
    toWidget $(luciusFile "templates/solver_result.lucius")
    -- $(widgetFile "empty")
    $(widgetFile "show_job_results")
