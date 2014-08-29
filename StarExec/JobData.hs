module StarExec.JobData
  ( queryJob
  , querySolverInfo
  , queryBenchmarkInfo
  , queryJobPair
  , queryManyJobs
  , queryPostProc
  ) where

import Import
import Presenter.Models
import StarExec.Types
import StarExec.Connection
import StarExec.Concurrent
import qualified StarExec.Commands as SEC
import StarExec.Persist
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time.Clock

updateThreshold :: Seconds
updateThreshold = 300

getTime :: Handler UTCTime
getTime = liftIO getCurrentTime

queryJob :: JobID -> Handler (QueryResult QueryInfo (Maybe Job, [JobResult]))
queryJob j@(LriJobID _jobId) = do
  mPersistJobInfo <- getPersistJobInfo' j
  case mPersistJobInfo
queryJob j@(StarExecJobID _jobId) = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  currentTime <- getTime
  case mPersistJobInfo of
    Just persistJobInfo -> do
      let since = diffTime currentTime $ jobInfoLastUpdate persistJobInfo
          jobComplete = Complete == jobInfoStatus persistJobInfo
      --liftIO $ putStrLn $ "### queryJob: " ++ (show _jobId) ++ " status: " ++ (show $ jobInfoStatus persistJobInfo)
      --liftIO $ putStrLn $ "### queryJob: " ++ (show _jobId) ++ " too old?: " ++ (show $ since > updateThreshold)
      if (not jobComplete) || (since > updateThreshold)
        then runQueryJob _jobId
        else do
          persistJobResults <- getPersistJobResults _jobId
          return $ QueryResult Latest (Just persistJobInfo, persistJobResults)
    Nothing -> runQueryJob _jobId

querySolverInfo :: Int -> Handler (QueryResult QueryInfo (Maybe SolverInfo))
querySolverInfo _solverId = do
  mPersistSolverInfo <- getPersistSolverInfo _solverId
  currentTime <- getTime
  case mPersistSolverInfo of
    Just persistSolverInfo -> do
      let since = diffTime currentTime $ solverInfoLastUpdate persistSolverInfo
      if since > updateThreshold
        then runQuerySolverInfo _solverId
        else return $ QueryResult Latest $ Just persistSolverInfo
    Nothing -> runQuerySolverInfo _solverId

queryBenchmarkInfo :: Int -> Handler (QueryResult QueryInfo (Maybe BenchmarkInfo))
queryBenchmarkInfo _benchmarkId = do
  mPersistBenchmarkInfo <- getPersistBenchmarkInfo _benchmarkId
  currentTime <- getTime
  case mPersistBenchmarkInfo of
    Just persistBenchmarkInfo -> do
      let since = diffTime currentTime $ benchmarkInfoLastUpdate persistBenchmarkInfo
      if since > updateThreshold
        then runQueryBenchmarkInfo _benchmarkId
        else return $ QueryResult Latest $ Just persistBenchmarkInfo
    Nothing -> runQueryBenchmarkInfo _benchmarkId

queryJobPair :: Int -> Handler (QueryResult QueryInfo (Maybe JobPairInfo))
queryJobPair _pairId = do
  mPersistPairInfo <- getPersistJobPair _pairId
  case mPersistPairInfo of
    Just persistPairInfo -> do
      if jobPairInfoResultStatus persistPairInfo == JobResultComplete
        then return $ QueryResult Latest $ Just persistPairInfo
        else runQueryJobPair _pairId
    Nothing -> runQueryJobPair _pairId

queryPostProc :: Int -> Handler (QueryResult QueryInfo (Maybe PostProcInfo))
queryPostProc _procId = do
  mPersistPostProcInfo <- getPersistPostProcInfo _procId
  currentTime <- getTime
  case mPersistPostProcInfo of
    Just persistPostProcInfo -> do
      let since = diffTime currentTime $ postProcInfoLastUpdate persistPostProcInfo
      if since > updateThreshold
        then runQueryPostProcInfo _procId
        else return $ QueryResult Latest $ Just persistPostProcInfo
    Nothing -> runQueryPostProcInfo _procId

queryManyJobs :: [JobID] -> Handler [QueryResult QueryInfo (Maybe Job, [JobResult])]
queryManyJobs = mapM queryJob
