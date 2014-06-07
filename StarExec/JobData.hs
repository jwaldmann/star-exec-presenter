module StarExec.JobData where

import Import
import StarExec.Types
import StarExec.Connection
import StarExec.Commands
import StarExec.Persist
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Codec.Compression.GZip

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

getJobResultsWithConnection :: ( MonadHandler m, MonadBaseControl IO m )
  => StarExecConnection -> Int -> m [JobResultInfo]
getJobResultsWithConnection con _jobId = do
  mInfo <- getJobInfo con _jobId
  return $ case mInfo of
    Just info -> info
    Nothing   -> []

getJobResultsFromStarExec :: ( MonadHandler m, MonadBaseControl IO m )
  => Int -> m [JobResultInfo]
getJobResultsFromStarExec _jobId = do
  con <- getConnection
  getJobResultsWithConnection con _jobId

selectListByJobId _jobId = selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []

getJobResults _jobId = do
  pResults <- runDB $ selectListByJobId _jobId
  if null pResults
    then do
      jobInfos <- getJobResultsFromStarExec _jobId
      pResults' <- runDB $ do
        _ <- mapM (dbInsertJobResult _jobId) jobInfos
        selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []
      return $ map entityVal pResults'
    else do
      return $ map entityVal pResults

getManyJobResults _jobIds = do
  pResults <- runDB $ mapM selectListByJobId _jobIds
  con <- if any null pResults
            then getConnection
            else return undefined
  let getJobResults' = getJobResultsWithConnection con
      processResult (_jobId, pResult) =
        if null pResult
          then do
            jobInfos <- getJobResults' _jobId
            pResults' <- runDB $ do
              _ <- mapM (dbInsertJobResult _jobId) jobInfos
              selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []
            return $ map entityVal pResults'
          else do
            return $ map entityVal pResult
  mapM processResult $ zip _jobIds pResults

getJobPair _pairId = runDB $ do
  mPair <- getBy $ UniquePersistJobPairInfo _pairId
  case mPair of
    Just pair -> return $ Just $ fromPersistJobPairInfo $ entityVal pair
    Nothing -> do
      con <- getConnection
      mPairInfo <- getJobPairInfo con _pairId
      case mPairInfo of
        Just pairInfo -> do
          mKey <- insertUnique $ PersistJobPairInfo
                                  _pairId
                                  (compressText $ jpiStdout pairInfo)
                                  (compressText $ jpiLog pairInfo)
          case mKey of
            Just key -> do
              mVal <- get key
              case mVal of
                Just val -> return $ Just $ fromPersistJobPairInfo val
                Nothing -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
