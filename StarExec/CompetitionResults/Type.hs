module StarExec.CompetitionResults.Type where

import Prelude
import Model
import StarExec.Types
import StarExec.Processing
import StarExec.Statistics
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock
import qualified Data.IntMap.Strict as IM

import qualified Data.Map as M

data CompetitionResults = CompetitionResults
  { competitionName :: Name
  , metaCategoryResults :: [MetaCategoryResult]
  , competitionComplete :: Bool 
  , competitionStartTime :: Maybe UTCTime
  , competitionFinishTime :: Maybe UTCTime
  , competitionStatistics :: Statistics
  } deriving (Show)

data MetaCategoryResult = MetaCategoryResult
  { metaCategoryName :: Name
  , categoryResults :: [CategoryResult]
  , metaCategoryRanking :: [(Maybe Rank, Solver, Score)]
  , metaCategoryComplete :: Bool
  , metaCategoryStarTime :: Maybe UTCTime
  , metaCategoryFinishTime :: Maybe UTCTime
  , metaCategoryStatistics :: Statistics
  } deriving (Show)

data CategoryResult = CategoryResult
  { categoryName :: Name
  , categoryScoring :: Scoring
  , categoryPostProc :: Maybe PostProcInfo
  , categoryRanking :: [(Maybe Rank, Solver, Score)]
  , categoryJobs :: [JobInfo]
  , categoryComplete :: Bool
  , categoryStartTime :: Maybe UTCTime
  , categoryFinishTime :: Maybe UTCTime
  , categoryStatistics :: Statistics
  } deriving (Show)
