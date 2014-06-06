module StarExec.JobResultsTable where

import Import
import StarExec.Types
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
