module StarExec.JobData where

import Import
import StarExec.Types
import StarExec.PersistTypes
import StarExec.Connection
import StarExec.Concurrent
import qualified StarExec.Commands as SEC
import StarExec.Persist
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time.Clock

type BenchmarkID = Int
type BenchmarkName = T.Text
type Benchmark = (BenchmarkID, BenchmarkName)
type SolverID = Int
type Solver = (SolverID, SolverName)
type SolverName = T.Text
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (Benchmark, [Maybe JobResultInfo])
type TableHead = [SolverName]

updateThreshold :: Seconds
updateThreshold = 300

getTime :: Handler UTCTime
getTime = liftIO getCurrentTime

getClass :: JobResultInfo -> T.Text
getClass result =
  case jobResultInfoResult result of
    YES       -> "solver-yes"
    NO        -> "solver-no"
    MAYBE     -> "solver-maybe"
    CERTIFIED -> "solver-certified"
    ERROR     -> "solver-error"
    _         -> "solver-nothing"

getInfo :: (JobResultInfo -> S.Set a -> S.Set a) -> [JobResultInfo] -> [a]
getInfo f = S.toList . L.foldr f S.empty

extractBenchmark :: JobResultInfo -> S.Set Benchmark -> S.Set Benchmark
extractBenchmark jri set =
  S.insert
    (jobResultInfoBenchmarkId jri, jobResultInfoBenchmark jri)
     set

extractSolver :: JobResultInfo -> S.Set Solver -> S.Set Solver
extractSolver jri set =
  S.insert
    (jobResultInfoSolverId jri, jobResultInfoSolver jri)
    set

getBenchmarkResults :: [Solver] -> [JobResultInfo] -> [Benchmark] -> [BenchmarkRow]
getBenchmarkResults solvers jobInfos = map getBenchmarkRow 
  where
    getBenchmarkRow benchmark@(bId, _) =
      (benchmark, map (getSolverResults bId) solvers)
    getSolverResults _benchmarkId (sId, _) =
      let mResult = L.find (isResult _benchmarkId sId) jobInfos
      in case mResult of
        Just result -> Just result
        Nothing -> Nothing
    isResult _benchmarkId _solverId jri =
      (jobResultInfoBenchmarkId jri == _benchmarkId) &&
        (jobResultInfoSolverId jri == _solverId)

compareBenchmarks :: Benchmark -> Benchmark -> Ordering
compareBenchmarks (_,n0) (_,n1) = compare n0 n1

getJobResultsWithConnection :: StarExecConnection -> Int -> Handler [JobResultInfo]
getJobResultsWithConnection con _jobId = do
  mResults <- SEC.getJobResults con _jobId
  return $ case mResults of
    Just result -> result
    Nothing     -> []

getJobResultsFromStarExec :: Int -> Handler [JobResultInfo]
getJobResultsFromStarExec _jobId = do
  con <- getConnection
  getJobResultsWithConnection con _jobId

queryJobResults :: Int -> Handler (QueryResult QueryInfo [JobResultInfo])
queryJobResults _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  currentTime <- getTime
  case mPersistJobInfo of
    Just persistJobInfo -> do
      let since = diffTime currentTime $ jobInfoLastUpdate persistJobInfo
      if since > updateThreshold
        then queryJobResults' _jobId
        else do
          persistJobResults <- getPersistJobResults _jobId
          return $ QueryResult Latest persistJobResults
    Nothing -> queryJobResults' _jobId
  where
    queryJobResults' :: Int -> Handler (QueryResult QueryInfo [JobResultInfo])
    queryJobResults' _jobId = do
      _ <- runQueryJobInfo _jobId
      runQueryJobResults _jobId

queryJobInfo :: Int -> Handler (QueryResult QueryInfo (Maybe JobInfo))
queryJobInfo _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  currentTime <- getTime
  case mPersistJobInfo of
    Just persistJobInfo -> do
      let since = diffTime currentTime $ jobInfoLastUpdate persistJobInfo
          jobComplete = Complete == jobInfoStatus persistJobInfo
      if not jobComplete || since > updateThreshold
        then runQueryJobInfo _jobId
        else return $ QueryResult Latest $ Just persistJobInfo
    Nothing -> runQueryJobInfo _jobId

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
      return $ QueryResult Latest $ Just persistPairInfo
    Nothing -> runQueryJobPair _pairId

getJobInfo :: Int -> Handler (Maybe JobInfo)
getJobInfo _jobId = do
  mPersistJobInfo <- runDB $ getBy $ UniqueJobInfo _jobId
  case mPersistJobInfo of
    Nothing -> do
      con <- getConnection
      mJobInfo <- SEC.getJobInfo con _jobId
      case mJobInfo of
        Nothing -> return Nothing
        Just ji -> return $ Just ji
    Just en -> return $ Just $ entityVal en

getJobResults :: Int -> Handler [JobResultInfo]
getJobResults _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  case mPersistJobInfo of
    Nothing -> do
      con <- getConnection
      mJobInfo <- SEC.getJobInfo con _jobId
      case mJobInfo of
        Nothing -> error $ "No such Job: " ++ (show _jobId)
        Just ji -> do
          if jobInfoStatus ji == Complete
            then do
              insertJobInfo ji
              jobResults <- getJobResultsWithConnection con _jobId
              mapM_ dbInsertJobResult jobResults
              getPersistJobResults _jobId
            else getJobResultsWithConnection con _jobId
    -- job is completed
    Just _ -> getPersistJobResults _jobId

getManyJobResults :: [Int] -> Handler [[JobResultInfo]] 
getManyJobResults = mapM getJobResults

getJobPair :: Int -> Handler (Maybe JobPairInfo)
getJobPair _pairId = do
  mPair <- runDB $ getBy $ UniqueJobPairInfo _pairId
  case mPair of
    Just pair -> return $ Just $ entityVal pair
    Nothing -> do
      con <- getConnection
      mPairInfo <- SEC.getJobPairInfo con _pairId
      case mPairInfo of
        Just pairInfo -> do
          mKey <- runDB $ insertUnique pairInfo
          case mKey of
            Just key -> do
              mVal <- runDB $ get key
              case mVal of
                Just val -> return $ Just val
                Nothing -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
