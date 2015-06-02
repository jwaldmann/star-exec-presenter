module Presenter.StarExec.JobData
  ( queryJob
  , querySolverInfo
  , queryBenchmarkInfo
  , queryJobPair
  , queryManyJobs
  , queryPostProc
  ) where

import Import
import Presenter.StarExec.Concurrent
import Presenter.PersistHelper
import Presenter.Prelude
import Data.Time.Clock

updateThreshold :: Seconds
updateThreshold = 300

getTime :: Handler UTCTime
getTime = liftIO getCurrentTime

wrap :: (a -> b) -> QueryResult QueryInfo a -> Handler (QueryResult QueryInfo b)
wrap f q = return $ QueryResult (queryStatus q) $ f (queryResult q)

queryJob :: JobID -> Handler (QueryResult QueryInfo (Maybe Job, [JobResult]))
queryJob _jobId@(StarExecJobID jid) = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  currentTime <- getTime
  case mPersistJobInfo of
    Just (StarExecJob persistJobInfo) -> do
      let since = diffTime currentTime $ jobInfoLastUpdate persistJobInfo
          jobComplete = Complete == jobInfoStatus persistJobInfo
      if (not jobComplete) || (since > updateThreshold)
        then runQueryJob jid >>= wrap'
        else do
          persistJobResults <- getPersistJobResults _jobId
          return $ QueryResult Latest (mPersistJobInfo, persistJobResults)
    _ -> runQueryJob jid >>= wrap'
  where
    wrap' = wrap $ \(mJobInfo, results) ->
      (StarExecJob <$> mJobInfo, map StarExecResult results)
queryJob _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  case mPersistJobInfo of
    Just _ -> do
      persistJobResults <- getPersistJobResults _jobId
      return $ QueryResult Latest (mPersistJobInfo, persistJobResults)
    Nothing -> return $ QueryResult Latest (Nothing, [])

querySolverInfo :: SolverID -> Handler (QueryResult QueryInfo (Maybe Solver))
querySolverInfo _solverId@(StarExecSolverID sid) = do
  mPersistSolverInfo <- getPersistSolverInfo _solverId
  currentTime <- getTime
  case mPersistSolverInfo of
    Just (StarExecSolver persistSolverInfo) -> do
      let since = diffTime currentTime $ solverInfoLastUpdate persistSolverInfo
      if since > updateThreshold
        then runQuerySolverInfo sid >>= wrap (fmap StarExecSolver)
        else return $ QueryResult Latest mPersistSolverInfo
    _ -> runQuerySolverInfo sid >>= wrap (fmap StarExecSolver)
querySolverInfo _solverId = do
  mPersistSolverInfo <- getPersistSolverInfo _solverId
  return $ QueryResult Latest mPersistSolverInfo

queryBenchmarkInfo :: BenchmarkID -> Handler (QueryResult QueryInfo (Maybe Benchmark))
queryBenchmarkInfo _benchmarkId@(StarExecBenchmarkID bid) = do
  mPersistBenchmarkInfo <- getPersistBenchmarkInfo _benchmarkId
  currentTime <- getTime
  case mPersistBenchmarkInfo of
    Just (StarExecBenchmark persistBenchmarkInfo) -> do
      let since = diffTime currentTime $ benchmarkInfoLastUpdate persistBenchmarkInfo
      if since > updateThreshold
        then runQueryBenchmarkInfo bid >>= wrap (fmap StarExecBenchmark)
        else return $ QueryResult Latest mPersistBenchmarkInfo
    _ -> runQueryBenchmarkInfo bid >>= wrap (fmap StarExecBenchmark)
queryBenchmarkInfo _benchmarkId = do
  mPersistBenchmarkInfo <- getPersistBenchmarkInfo _benchmarkId
  return $ QueryResult Latest mPersistBenchmarkInfo

queryJobPair :: JobPairID -> Handler (QueryResult QueryInfo (Maybe Pair))
queryJobPair _pairId@(StarExecPairID pid) = do
  mPersistPairInfo <- getPersistJobPair _pairId
  case mPersistPairInfo of
    Just (StarExecPair persistPairInfo) -> do
      if jobPairInfoResultStatus persistPairInfo == JobResultComplete
        then return $ QueryResult Latest mPersistPairInfo
        else runQueryJobPair pid >>= wrap (fmap StarExecPair)
    _ -> runQueryJobPair pid >>= wrap (fmap StarExecPair)
queryJobPair _pairId = do
  mPersistPairInfo <- getPersistJobPair _pairId
  return $ QueryResult Latest mPersistPairInfo

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
