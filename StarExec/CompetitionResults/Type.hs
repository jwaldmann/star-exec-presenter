module StarExec.CompetitionResults.Type where

import Prelude
import Model
import Presenter.Models
import StarExec.Types
import StarExec.Processing
import StarExec.Statistics
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock
import qualified Data.IntMap.Strict as IM

import qualified Data.Map as M

data SolverRankEntry = SolverRankEntry
  { rank :: Maybe Rank
  , solver :: Solver
  , score :: Score
  } deriving (Eq, Show)

data CompetitionResults = CompetitionResults
  { competitionMeta :: CompetitionMeta
  , metaCategoryResults :: [MetaCategoryResult]
  , competitionComplete :: Bool 
  , competitionStartTime :: Maybe UTCTime
  , competitionFinishTime :: Maybe UTCTime
  , competitionStatistics :: Statistics
  } deriving (Show)

competitionName = getMetaName . competitionMeta
competitionDescription = getMetaDescription . competitionMeta

data MetaCategoryResult = MetaCategoryResult
  { metaCategoryName :: Name
  , categoryResults :: [CategoryResult]
  , metaCategoryRanking :: [SolverRankEntry]
  , metaCategoryComplete :: Bool
  , metaCategoryStarTime :: Maybe UTCTime
  , metaCategoryFinishTime :: Maybe UTCTime
  , metaCategoryStatistics :: Statistics
  } deriving (Show)

data CategoryResult = CategoryResult
  { categoryName :: Name
  , categoryScoring :: Scoring
  , categoryPostProc :: Maybe PostProcInfo
  , categoryRanking :: [SolverRankEntry]
  , categoryJobs :: [JobInfo]
  , categoryComplete :: Bool
  , categoryStartTime :: Maybe UTCTime
  , categoryFinishTime :: Maybe UTCTime
  , categoryStatistics :: Statistics
  } deriving (Show)
