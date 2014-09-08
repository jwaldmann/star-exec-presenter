module Presenter.StarExec.JobData
  ( queryJob
  , querySolverInfo
  , queryBenchmarkInfo
  , queryJobPair
  , queryManyJobs
  , queryPostProc
  ) where

import Import
import Presenter.StarExec.Connection
import Presenter.StarExec.Concurrent
import qualified Presenter.StarExec.Commands as SEC
import Presenter.PersistHelper
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time.Clock

updateThreshold :: Seconds
updateThreshold = 300

getTime :: Handler UTCTime
getTime = liftIO getCurrentTime

queryJob :: Int -> Handler (QueryResult QueryInfo (Maybe JobInfo, [JobResultInfo]))
queryJob _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  currentTime <- getTime
  case mPersistJobInfo of
    Just persistJobInfo -> do
      let since = diffTime currentTime $ jobInfoLastUpdate persistJobInfo
          jobComplete = Complete == jobInfoStatus persistJobInfo
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

queryManyJobs :: [Int] -> Handler [QueryResult QueryInfo (Maybe JobInfo, [JobResultInfo])]
queryManyJobs = mapM queryJob
