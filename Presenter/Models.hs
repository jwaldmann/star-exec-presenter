module Presenter.Models where

import Prelude
import Model
import Presenter.RouteTypes
import StarExec.Types (SolverResult, JobResultStatus (..))
import qualified Data.Text as T
import Data.Maybe

class ResultEntity a where
  getSolverResult :: a -> SolverResult
  toJobID :: a -> JobID
  toResultID :: a -> JobResultID
  isResultComplete :: a -> Bool
  updateScore :: a -> Maybe Int -> a

class BenchmarkEntity a where
  toBenchmarkID :: a -> BenchmarkID
  toBenchmarkName :: a -> T.Text

class SolverEntity a where
  toSolverID :: a -> SolverID
  toSolverName :: a -> T.Text

instance ResultEntity JobResult where
  getSolverResult (StarExecResult r) = getSolverResult r
  getSolverResult (LriResult r) = getSolverResult r

  toJobID (StarExecResult r) = toJobID r
  toJobID (LriResult r) = toJobID r

  toResultID (StarExecResult r) = toResultID r
  toResultID (LriResult r) = toResultID r

  isResultComplete (StarExecResult r) = isResultComplete r
  isResultComplete (LriResult r) = isResultComplete r

  updateScore (StarExecResult r) = StarExecResult . (updateScore r)
  updateScore (LriResult r) = LriResult . (updateScore r)

instance ResultEntity JobResultInfo where
  getSolverResult = jobResultInfoResult

  toJobID = StarExecJobID . jobResultInfoJobId

  toResultID = StarExecResultID . jobResultInfoPairId

  isResultComplete r = jobResultInfoStatus r == JobResultComplete

  updateScore r s = r { jobResultInfoScore = s }

instance ResultEntity LriResultInfo where
  getSolverResult = lriResultInfoResult

  toJobID = LriJobID . lriResultInfoJobId

  toResultID = LriResultID . lriResultInfoPairId

  isResultComplete _ = True

  updateScore r s = r { lriResultInfoScore = s }

data JobResult =
  StarExecResult JobResultInfo
  | LriResult LriResultInfo

class FromJobResult a where
  fromJobResult :: JobResult -> Maybe a

instance FromJobResult JobResultInfo where
  fromJobResult (StarExecResult r) = Just r
  fromJobResult _ = Nothing

instance FromJobResult LriResultInfo where
  fromJobResult (LriResult r) = Just r
  fromJobResult _ = Nothing

isStarExecResult :: JobResult -> Bool
isStarExecResult (StarExecResult _) = True
isStarExecResult _ = False

isLriResult :: JobResult -> Bool
isLriResult (LriResult _) = True
isLriResult _ = False

unwrapResults :: FromJobResult a => [JobResult] -> [a]
unwrapResults = catMaybes . (map fromJobResult)

newtype JobResults = JobResults
  { getResults :: [JobResult]
  }

data Benchmark =
  StarExecBenchmark BenchmarkInfo
  | LriBenchmark LriBenchmarkInfo
  deriving (Eq, Ord, Read, Show)

instance BenchmarkEntity Benchmark where
  toBenchmarkID (StarExecBenchmark b) = toBenchmarkID b
  toBenchmarkID (LriBenchmark b) = toBenchmarkID b

  toBenchmarkName (StarExecBenchmark b) = toBenchmarkName b
  toBenchmarkName (LriBenchmark b) = toBenchmarkName b

instance BenchmarkEntity JobResult where
  toBenchmarkID (StarExecResult r) = toBenchmarkID r
  toBenchmarkID (LriResult r) = toBenchmarkID r

  toBenchmarkName (StarExecResult r) = toBenchmarkName r
  toBenchmarkName (LriResult r) = toBenchmarkName r

instance BenchmarkEntity JobResultInfo where
  toBenchmarkID = StarExecBenchmarkID . jobResultInfoBenchmarkId

  toBenchmarkName = jobResultInfoBenchmark

instance BenchmarkEntity LriResultInfo where
  toBenchmarkID = LriBenchmarkID . lriResultInfoBenchmarkId

  toBenchmarkName _ = ""

instance BenchmarkEntity BenchmarkInfo where
  toBenchmarkID = StarExecBenchmarkID . benchmarkInfoStarExecId

  toBenchmarkName = benchmarkInfoName

instance BenchmarkEntity LriBenchmarkInfo where
  toBenchmarkID = LriBenchmarkID . lriBenchmarkInfoBenchmarkId

  toBenchmarkName = lriBenchmarkInfoName

newtype Benchmarks = Benchmarks
  { getBenchmarks :: [Benchmark]
  }

data Solver =
  StarExecSolver SolverInfo
  | LriSolver LriSolverInfo
  deriving (Eq, Ord, Read, Show)

instance SolverEntity Solver where
  toSolverID (StarExecSolver s) = toSolverID s
  toSolverID (LriSolver s) = toSolverID s

  toSolverName (StarExecSolver s) = toSolverName s
  toSolverName (LriSolver s) = toSolverName s

instance SolverEntity JobResult where
  toSolverID (StarExecResult r) = toSolverID r
  toSolverID (LriResult r) = toSolverID r

  toSolverName (StarExecResult r) = toSolverName r
  toSolverName (LriResult r) = toSolverName r

instance SolverEntity JobResultInfo where
  toSolverID = StarExecSolverID . jobResultInfoSolverId

  toSolverName = jobResultInfoSolver

instance SolverEntity LriResultInfo where
  toSolverID = LriSolverID . lriResultInfoSolverId

  toSolverName _ = ""

instance SolverEntity SolverInfo where
  toSolverID = StarExecSolverID . solverInfoStarExecId

  toSolverName = solverInfoName

instance SolverEntity LriSolverInfo where
  toSolverID = LriSolverID . lriSolverInfoSolverId

  toSolverName = lriSolverInfoName

newtype Solvers = Solvers
  { getSolvers :: [Solver]
  }


