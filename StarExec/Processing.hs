module StarExec.Processing where

import Import
import StarExec.Types
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

type BenchmarkID = Int
type BenchmarkName = T.Text
type Benchmark = (BenchmarkID, BenchmarkName)
type SolverID = Int
type Solver = (SolverID, SolverName)
type SolverName = T.Text
type SolverResults = [Maybe SolverResult]
type BenchmarkRow = (Benchmark, [Maybe JobResultInfo])
type TableHead = [SolverName]

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
