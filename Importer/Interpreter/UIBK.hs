module Importer.Interpreter.UIBK
  ( getBenchmarks
  , getSolvers
  ) where

import Prelude
import qualified Data.List as L
import Importer.Internal

getBenchmarks :: [UIBKResult] -> [UIBKBenchmark]
getBenchmarks = L.nub . L.map createBenchmark
  where
    createBenchmark r = UIBKBenchmark
      { uibkBenchmarkID = uibkResultInputProblemID r
      , uibkBenchmarkPath = uibkResultInputProblemPath r
      }

getSolvers :: [UIBKResult] -> [UIBKSolver]
getSolvers = L.nub . L.map createSolver
  where
    createSolver r = UIBKSolver
      { uibkSolverName = uibkResultTool r
      , uibkSolverVersion = uibkResultToolVersion r
      }
