module Presenter.Model.CompetitionResults where

import Prelude (Eq, Show, Bool, Maybe, (.))
import Model
import Presenter.Model.Types
import Presenter.Model.Competition
import Presenter.Model.Entities
import Presenter.Model.RouteTypes
import Presenter.Statistics
import Data.Time.Clock

type SolverName = Name
type UniqueSolver = (SolverID, SolverName)

data SolverRankEntry = SolverRankEntry
  { rank :: Maybe Rank
  , solver :: UniqueSolver
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

competitionName :: CompetitionResults -> Name
competitionName = getMetaName . competitionMeta

competitionDescription :: CompetitionResults -> Description
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
  , categoryJobs :: [Job]
  , categoryComplete :: Bool
  , categoryStartTime :: Maybe UTCTime
  , categoryFinishTime :: Maybe UTCTime
  , categoryStatistics :: Statistics
  } deriving (Show)
