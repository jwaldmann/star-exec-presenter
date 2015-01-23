module Importer.Interpreter.UIBK
  ( getBenchmarks
  , getSolvers
  ) where

import Prelude
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as Char8
import Importer.Internal
import Control.Applicative
import Control.Monad

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
