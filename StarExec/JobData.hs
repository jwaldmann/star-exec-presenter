module StarExec.JobData
  ( queryJob
  , querySolverInfo
  , queryBenchmarkInfo
  , queryJobPair
  , queryManyJobs
  , getClass
  , getBenchmarkResults
  , getInfo
  , extractBenchmark
  , extractSolver
  , compareBenchmarks
  , Solver
  , SolverName
  , SolverID
  , SolverResults
  , Benchmark
  , BenchmarkID
  , BenchmarkName
  , BenchmarkRow
  , TableHead
  ) where

import Import
import StarExec.Types
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
    YES _     -> "solver-yes"
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

queryJob :: Int -> Handler (QueryResult QueryInfo (Maybe JobInfo, [JobResultInfo]))
queryJob _jobId = do
  mPersistJobInfo <- getPersistJobInfo _jobId
  currentTime <- getTime
  case mPersistJobInfo of
    Just persistJobInfo -> do
      let since = diffTime currentTime $ jobInfoLastUpdate persistJobInfo
      if since > updateThreshold
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

queryManyJobs :: [Int] -> Handler [QueryResult QueryInfo (Maybe JobInfo, [JobResultInfo])]
queryManyJobs = mapM queryJob
