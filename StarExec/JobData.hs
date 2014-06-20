module StarExec.JobData where

import Import
import StarExec.Types
import StarExec.Connection
import qualified StarExec.Commands as SEC
import StarExec.Persist
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

type JobResultInfos = [JobResultInfo]
type BenchmarkID = Int
type BenchmarkName = T.Text
type Benchmark = (BenchmarkID, BenchmarkName)
type Benchmarks = [Benchmark]
type SolverID = Int
type Solver = (SolverID, SolverName)
type Solvers = [Solver]
type SolverName = T.Text
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (Benchmark, [Maybe JobResultInfo])
type BenchmarkRows = [BenchmarkRow]
type TableHead = [SolverName]

getClass :: JobResultInfo -> T.Text
getClass result =
  case jriResult result of
    YES       -> "solver-yes"
    NO        -> "solver-no"
    MAYBE     -> "solver-maybe"
    CERTIFIED -> "solver-certified"
    ERROR     -> "solver-error"
    _         -> "solver-nothing"

getInfo :: (JobResultInfo -> S.Set a -> S.Set a) -> JobResultInfos -> [a]
getInfo f = S.toList . L.foldr f S.empty

extractBenchmark :: JobResultInfo -> S.Set Benchmark -> S.Set Benchmark
extractBenchmark jri set = S.insert (jriBenchmarkId jri, jriBenchmark jri) set

extractSolver :: JobResultInfo -> S.Set Solver -> S.Set Solver
extractSolver jri set = S.insert (jriSolverId jri, jriSolver jri) set

getBenchmarkResults :: Solvers -> JobResultInfos -> Benchmarks -> BenchmarkRows
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
      (jriBenchmarkId jri == _benchmarkId) && (jriSolverId jri == _solverId)

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

selectListByJobId :: Int -> Handler [PersistJobResultInfo]
selectListByJobId _jobId = runDB $ do
  results <- selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []
  return $ map entityVal results

getJobInfo :: Int -> Handler (Maybe JobInfo)
getJobInfo _jobId = do
  mPersistJobInfo <- runDB $ getBy $ UniquePersistJobInfo _jobId
  case mPersistJobInfo of
    Nothing -> do
      con <- getConnection
      mJobInfo <- SEC.getJobInfo con _jobId
      case mJobInfo of
        Nothing -> return Nothing
        Just ji -> return $ Just ji
    Just en -> do
      let persistJobInfo = entityVal en
      return $ Just $ JobInfo
        { jobId = persistJobInfoStarExecId persistJobInfo
        , jobName = persistJobInfoName persistJobInfo
        , jobStatus = persistJobInfoStatus persistJobInfo
        , jobDate = persistJobInfoDate persistJobInfo
        }

getJobResults :: Int -> Handler [PersistJobResultInfo]
getJobResults _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  case mPersistJobInfo of
    Nothing -> do
      con <- getConnection
      mJobInfo <- SEC.getJobInfo con _jobId
      case mJobInfo of
        Nothing -> error $ "No such Job: " ++ (show _jobId)
        Just ji -> do
          if jobStatus ji == Complete
            then do
              insertJobInfo ji
              jobResults <- getJobResultsWithConnection con _jobId
              mapM_ (dbInsertJobResult _jobId) jobResults
              selectListByJobId _jobId
            else do
              jobResults <- getJobResultsWithConnection con _jobId
              return $ map (toPersistJobResultInfo _jobId) jobResults
    -- job is completed
    Just _ -> selectListByJobId _jobId

getManyJobResults :: [Int] -> Handler [[PersistJobResultInfo]] 
getManyJobResults = mapM getJobResults

getJobPair :: Int -> Handler (Maybe JobPairInfo)
getJobPair _pairId = do
  mPair <- runDB $ getBy $ UniquePersistJobPairInfo _pairId
  case mPair of
    Just pair -> return $ Just $ fromPersistJobPairInfo $ entityVal pair
    Nothing -> do
      con <- getConnection
      mPairInfo <- SEC.getJobPairInfo con _pairId
      case mPairInfo of
        Just pairInfo -> do
          mKey <- runDB $ insertUnique $ PersistJobPairInfo
                                  _pairId
                                  (compressText $ jpiStdout pairInfo)
                                  (compressText $ jpiLog pairInfo)
          case mKey of
            Just key -> do
              mVal <- runDB $ get key
              case mVal of
                Just val -> return $ Just $ fromPersistJobPairInfo val
                Nothing -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
