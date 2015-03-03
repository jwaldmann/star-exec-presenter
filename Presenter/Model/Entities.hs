module Presenter.Model.Entities where

import Prelude
import Model
import Presenter.Prelude
import Presenter.Model.RouteTypes
import Presenter.Model.StarExec
  ( SolverResult
  , JobResultStatus (..)
  , JobStatus(..))
import Presenter.Model.Types (Seconds, Name)
import Data.Maybe
import Control.Applicative
import Data.Time.Clock ( UTCTime )

-- ###### TYPE-CLASSES ######

class ResultEntity a where
  getSolverResult :: a -> SolverResult
  getResultStatus :: a -> JobResultStatus
  getJobID :: a -> JobID
  getPairID :: a -> JobPairID
  --toResultID :: a -> JobResultID
  isResultComplete :: a -> Bool
  updateScore :: a -> Maybe Int -> a
  toScore :: a -> Maybe Int
  toCpuTime :: a -> Double
  toWallclockTime :: a -> Double

class BenchmarkEntity a where
  toBenchmarkID :: a -> BenchmarkID
  toBenchmarkName :: a -> Name

class SolverEntity a where
  toSolverID :: a -> SolverID
  toSolverName :: a -> Name

class ConfigEntity a where
  toConfigID :: a -> ConfigID
  toConfigName :: a -> Name

class JobEntity a where
  toJobName :: a -> Name
  toJobID :: a -> JobID
  toJobStatus :: a -> JobStatus
  toJobDuration :: a -> Maybe Seconds
  toJobStartDate :: a -> Maybe UTCTime
  toJobFinishDate :: a -> Maybe UTCTime
  isComplexity :: a -> Bool

class FromJobResult a where
  fromJobResult :: JobResult -> Maybe a
  toJobResult :: a -> JobResult
  unwrapResults :: [JobResult] -> [a]
  unwrapResults = catMaybes . (map fromJobResult)
  wrapResults :: [a] -> [JobResult]
  wrapResults = map toJobResult

-- ###### DATA-TYPES ######

data Job =
  StarExecJob JobInfo
  | LriJob LriJobInfo
  | UibkJob UibkJobInfo
  deriving (Eq, Ord, Read, Show)

newtype Jobs = Jobs
  { getJobs :: [Job]
  }

data JobResult =
  StarExecResult JobResultInfo
  | LriResult LriResultInfo
  | UibkResult UibkResultInfo
  deriving (Eq, Ord, Read, Show)

newtype JobResults = JobResults
  { getResults :: [JobResult]
  }

data Pair  =
  StarExecPair JobPairInfo
  deriving (Eq, Ord, Read, Show)

data Benchmark =
  StarExecBenchmark BenchmarkInfo
  | LriBenchmark LriBenchmarkInfo
  | UibkBenchmark UibkBenchmarkInfo
  deriving (Eq, Ord, Read, Show)

newtype Benchmarks = Benchmarks
  { getBenchmarks :: [Benchmark]
  }

data Solver =
  StarExecSolver SolverInfo
  | LriSolver LriSolverInfo
  | UibkSolver UibkSolverInfo
  deriving (Eq, Ord, Read, Show)

newtype Solvers = Solvers
  { getSolvers :: [Solver]
  }

data ConfigID = 
  StarExecConfigID Int
  | LriConfigID
  deriving (Eq, Ord, Read, Show)

lriConfigName :: Name
lriConfigName = "lri_config"

-- ###### INSTANCES ######

-- #### ResultEntity ####

instance ResultEntity JobResult where
  getSolverResult (StarExecResult r) = getSolverResult r
  getSolverResult (LriResult r) = getSolverResult r
  getSolverResult (UibkResult r) = getSolverResult r

  getResultStatus (StarExecResult r) = getResultStatus r
  getResultStatus (LriResult r) = getResultStatus r
  getResultStatus (UibkResult r) = getResultStatus r

  getJobID (StarExecResult r) = getJobID r
  getJobID (LriResult r) = getJobID r
  getJobID (UibkResult r) = getJobID r

  getPairID (StarExecResult r) = getPairID r
  getPairID (LriResult r) = getPairID r
  getPairID (UibkResult r) = getPairID r

  --toResultID (StarExecResult r) = toResultID r
  --toResultID (LriResult r) = toResultID r

  isResultComplete (StarExecResult r) = isResultComplete r
  isResultComplete (LriResult r) = isResultComplete r
  isResultComplete (UibkResult r) = isResultComplete r

  updateScore (StarExecResult r) = StarExecResult . (updateScore r)
  updateScore (LriResult r) = LriResult . (updateScore r)
  updateScore (UibkResult r) = UibkResult . (updateScore r)

  toScore (StarExecResult r) = toScore r
  toScore (LriResult r) = toScore r
  toScore (UibkResult r) = toScore r

  toCpuTime (StarExecResult r) = toCpuTime r
  toCpuTime (LriResult r) = toCpuTime r
  toCpuTime (UibkResult r) = toCpuTime r

  toWallclockTime (StarExecResult r) = toWallclockTime r
  toWallclockTime (LriResult r) = toWallclockTime r
  toWallclockTime (UibkResult r) = toWallclockTime r

instance ResultEntity JobResultInfo where
  getSolverResult = jobResultInfoResult

  getResultStatus = jobResultInfoStatus

  getJobID = StarExecJobID . jobResultInfoJobId

  getPairID = StarExecPairID . jobResultInfoPairId

  --toResultID = StarExecResultID . jobResultInfoPairId

  isResultComplete r = jobResultInfoStatus r == JobResultComplete

  updateScore r s = r { jobResultInfoScore = s }

  toScore = jobResultInfoScore

  toCpuTime = jobResultInfoCpuTime

  toWallclockTime = jobResultInfoWallclockTime

instance ResultEntity LriResultInfo where
  getSolverResult = lriResultInfoResult

  getResultStatus _ = JobResultComplete

  getJobID = LriJobID . lriResultInfoJobId

  getPairID = LriPairID . lriResultInfoPairId

  --toResultID = LriResultID . lriResultInfoPairId

  isResultComplete _ = True

  updateScore r s = r { lriResultInfoScore = s }

  toScore = lriResultInfoScore

  toCpuTime = lriResultInfoCpuTime

  toWallclockTime = lriResultInfoWallclockTime

instance ResultEntity UibkResultInfo where
  getSolverResult = uibkResultInfoResult

  getResultStatus _ = JobResultComplete

  getJobID = UibkJobID . uibkResultInfoJobId

  getPairID = UibkPairID . uibkResultInfoPairId

  --toResultID = LriResultID . lriResultInfoPairId

  isResultComplete _ = True

  updateScore r s = r { uibkResultInfoScore = s }

  toScore = uibkResultInfoScore

  toCpuTime _ = -1

  toWallclockTime = uibkResultInfoWallclockTime

-- #### FromJobResult ####

instance FromJobResult JobResultInfo where
  fromJobResult (StarExecResult r) = Just r
  fromJobResult _ = Nothing
  toJobResult = StarExecResult

instance FromJobResult LriResultInfo where
  fromJobResult (LriResult r) = Just r
  fromJobResult _ = Nothing
  toJobResult = LriResult

instance FromJobResult UibkResultInfo where
  fromJobResult (UibkResult r) = Just r
  fromJobResult _ = Nothing
  toJobResult = UibkResult

-- #### BenchmarkEntity ####

instance BenchmarkEntity Benchmark where
  toBenchmarkID (StarExecBenchmark b) = toBenchmarkID b
  toBenchmarkID (LriBenchmark b) = toBenchmarkID b
  toBenchmarkID (UibkBenchmark b) = toBenchmarkID b

  toBenchmarkName (StarExecBenchmark b) = toBenchmarkName b
  toBenchmarkName (LriBenchmark b) = toBenchmarkName b
  toBenchmarkName (UibkBenchmark b) = toBenchmarkName b

instance BenchmarkEntity JobResult where
  toBenchmarkID (StarExecResult r) = toBenchmarkID r
  toBenchmarkID (LriResult r) = toBenchmarkID r
  toBenchmarkID (UibkResult r) = toBenchmarkID r

  toBenchmarkName (StarExecResult r) = toBenchmarkName r
  toBenchmarkName (LriResult r) = toBenchmarkName r
  toBenchmarkName (UibkResult r) = toBenchmarkName r

instance BenchmarkEntity JobResultInfo where
  toBenchmarkID = StarExecBenchmarkID . jobResultInfoBenchmarkId

  toBenchmarkName = jobResultInfoBenchmark

instance BenchmarkEntity LriResultInfo where
  toBenchmarkID = LriBenchmarkID . lriResultInfoBenchmarkId

  toBenchmarkName = lriResultInfoBenchmark

instance BenchmarkEntity UibkResultInfo where
  toBenchmarkID = UibkBenchmarkID . uibkResultInfoBenchmarkId

  toBenchmarkName = uibkResultInfoBenchmarkName

instance BenchmarkEntity BenchmarkInfo where
  toBenchmarkID = StarExecBenchmarkID . benchmarkInfoStarExecId

  toBenchmarkName = benchmarkInfoName

instance BenchmarkEntity LriBenchmarkInfo where
  toBenchmarkID = LriBenchmarkID . lriBenchmarkInfoBenchmarkId

  toBenchmarkName = lriBenchmarkInfoName

instance BenchmarkEntity UibkBenchmarkInfo where
  toBenchmarkID = UibkBenchmarkID . uibkBenchmarkInfoBenchmarkId

  toBenchmarkName = uibkBenchmarkInfoPath

-- #### SolverEntity ####

instance SolverEntity Solver where
  toSolverID (StarExecSolver s) = toSolverID s
  toSolverID (LriSolver s) = toSolverID s
  toSolverID (UibkSolver s) = toSolverID s

  toSolverName (StarExecSolver s) = toSolverName s
  toSolverName (LriSolver s) = toSolverName s
  toSolverName (UibkSolver s) = toSolverName s

instance SolverEntity JobResult where
  toSolverID (StarExecResult r) = toSolverID r
  toSolverID (LriResult r) = toSolverID r
  toSolverID (UibkResult r) = toSolverID r

  toSolverName (StarExecResult r) = toSolverName r
  toSolverName (LriResult r) = toSolverName r
  toSolverName (UibkResult r) = toSolverName r

instance SolverEntity JobResultInfo where
  toSolverID = StarExecSolverID . jobResultInfoSolverId

  toSolverName = jobResultInfoSolver

instance SolverEntity LriResultInfo where
  toSolverID = LriSolverID . lriResultInfoSolverId

  toSolverName = lriResultInfoSolver

instance SolverEntity UibkResultInfo where
  toSolverID = UibkSolverID . uibkResultInfoSolverId

  toSolverName = uibkResultInfoSolverName

instance SolverEntity SolverInfo where
  toSolverID = StarExecSolverID . solverInfoStarExecId

  toSolverName = solverInfoName

instance SolverEntity LriSolverInfo where
  toSolverID = LriSolverID . lriSolverInfoSolverId

  toSolverName = lriSolverInfoName

instance SolverEntity UibkSolverInfo where
  toSolverID = UibkSolverID . uibkSolverInfoSolverId

  toSolverName = uibkSolverInfoName

-- #### ConfigEntity ####

instance ConfigEntity JobResult where
  toConfigID (StarExecResult r) = toConfigID r
  toConfigID (LriResult r) = toConfigID r

  toConfigName (StarExecResult r) = toConfigName r
  toConfigName (LriResult r) = toConfigName r

instance ConfigEntity JobResultInfo where
  toConfigID = StarExecConfigID . jobResultInfoConfigurationId

  toConfigName = jobResultInfoConfiguration

instance ConfigEntity LriResultInfo where
  toConfigID _ = LriConfigID

  toConfigName _ = lriConfigName

-- #### JobEntity ####

instance JobEntity Job where
  toJobName (StarExecJob j) = toJobName j
  toJobName (LriJob j) = toJobName j
  toJobName (UibkJob j) = toJobName j

  toJobID (StarExecJob j) = toJobID j
  toJobID (LriJob j) = toJobID j
  toJobID (UibkJob j) = toJobID j

  toJobStatus (StarExecJob j) = toJobStatus j
  toJobStatus _ = Complete

  toJobDuration (StarExecJob j) = toJobDuration j
  toJobDuration _ = Nothing

  toJobStartDate ( StarExecJob j ) = toJobStartDate j
  toJobStartDate _ = Nothing

  toJobFinishDate ( StarExecJob j ) = toJobFinishDate j
  toJobFinishDate _ = Nothing

  isComplexity (StarExecJob j) = isComplexity j
  isComplexity _ = False

instance JobEntity JobInfo where
  toJobName = jobInfoName

  toJobID = StarExecJobID . jobInfoStarExecId

  toJobStatus = jobInfoStatus

  toJobDuration j =
    diffTime <$> (jobInfoFinishDate j) <*> Just (jobInfoStartDate j)

  toJobStartDate = Just . jobInfoStartDate

  toJobFinishDate = jobInfoFinishDate

  isComplexity = jobInfoIsComplexity

instance JobEntity LriJobInfo where
  toJobName = lriJobInfoName

  toJobID = LriJobID . lriJobInfoJobId

  toJobStatus _ = Complete

  toJobDuration _ = Nothing

  toJobStartDate _ = Nothing

  toJobFinishDate _ = Nothing

  isComplexity _ = False

instance JobEntity UibkJobInfo where
  toJobName = uibkJobInfoName

  toJobID = UibkJobID . uibkJobInfoJobId

  toJobStatus _ = Complete

  toJobDuration _ = Nothing

  toJobStartDate _ = Nothing

  toJobFinishDate _ = Nothing

  isComplexity _ = False

-- ###### HELPER ######

isStarExecJob :: Job -> Bool
isStarExecJob (StarExecJob _) = True
isStarExecJob _ = False

isStarExecResult :: JobResult -> Bool
isStarExecResult (StarExecResult _) = True
isStarExecResult _ = False

isLriResult :: JobResult -> Bool
isLriResult (LriResult _) = True
isLriResult _ = False

getStarExecJobs :: [Job] -> [JobInfo]
getStarExecJobs (StarExecJob j:js) = j:getStarExecJobs js
getStarExecJobs (_:js) = getStarExecJobs js
getStarExecJobs [] = []

getStarExecResults :: [JobResult] -> [JobResultInfo]
getStarExecResults (StarExecResult r:rs) = r:getStarExecResults rs
getStarExecResults (_:rs) = getStarExecResults rs
getStarExecResults [] = []
