module Handler.ShowJobInfo where

import Import
import qualified StarExec.Commands as SEC
import StarExec.Types
import StarExec.Session
import StarExec.Persist
--import Data.Double.Conversion.Text
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

type JobResultInfos = [JobResultInfo]
type BenchmarkID = Int
type BenchmarkName = T.Text
type Benchmark = (BenchmarkID, BenchmarkName)
type SolverID = Int
type Solver = (SolverID, SolverName)
type SolverName = T.Text
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (Benchmark, SolverResults)
type TableHead = [SolverName]

getClass :: SolverResult -> T.Text
getClass result =
  case result of
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

getBenchmarkResults :: [Benchmark] -> [Solver] -> JobResultInfos -> [BenchmarkRow]
getBenchmarkResults benchmarks solvers jobInfos = map getBenchmarkRow benchmarks
  where
    getBenchmarkRow benchmark@(bId, _) =
      (benchmark, map (getSolverResults bId) solvers)
    getSolverResults _benchmarkId (sId, _) =
      let mResult = L.find (isResult _benchmarkId sId) jobInfos
      in case mResult of
        Just result -> Just $ jriResult result
        Nothing -> Nothing
    isResult _benchmarkId _solverId jri =
      (jriBenchmarkId jri == _benchmarkId) && (jriSolverId jri == _solverId)

compareBenchmarks :: Benchmark -> Benchmark -> Ordering
compareBenchmarks (_,n0) (_,n1) = compare n0 n1

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR _jobId = do
  loggedIn <- hasValidSession
  if not loggedIn
    then redirect HomeR
    else do
      pJobInfos <- getJobResults _jobId
      let 
        jobinfos = fromPersistJobResultInfos pJobInfos
        benchmarks = getInfo extractBenchmark jobinfos
        solvers = getInfo extractSolver jobinfos
        benchmarkResults = getBenchmarkResults
                            (L.sortBy compareBenchmarks benchmarks)
                            solvers
                            jobinfos
        solverNames = map snd solvers
      --liftIO $ putStrLn $ show $ length benchmarkResults
      --liftIO $ mapM (putStrLn . show) benchmarkResults
      defaultLayout $ do
        $(widgetFile "show_job_info")
