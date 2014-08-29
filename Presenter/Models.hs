module Presenter.Models where

import Import
import Importer.LRI

data JobResult =
  StarExecResult JobResultInfo
  | LriResult LRIResult

newtype JobResults = JobResults
  { getResults :: [JobResult]
  }

data Benchmark =
  StarExecBenchmark BenchmarkInfo
  | LriBenchmark LRIBenchmark

newtype Benchmarks = Benchmarks
  { getBenchmarks :: [Benchmark]
  }

data Solver =
  StarExecSolver SolverInfo
  | LriSolver LRISolver

newtype Solvers = Solvers
  { getSolvers :: [Solver]
  }


