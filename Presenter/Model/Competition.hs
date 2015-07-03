module Presenter.Model.Competition where

import Yesod
import Presenter.Model.Types
import Presenter.Model.RouteTypes
import Prelude (Show, Read, Eq, Ord, ($), (.), show, reads, return, Maybe (..))
import qualified Data.Text as T

data Scoring =
  Standard
  | Complexity
  deriving (Show, Read, Eq)

{-
  solver sorted by YES/CERTIFIED/NO, maybe with scoring -> SolverResult
-}
data Category = Category
  { getCategoryName :: Name
  , getCategoryScoring :: Scoring
  , getPostProcId :: PostProcId
  , getJobIds :: [JobID]
  } deriving (Show, Read, Eq)

{-
  solver by rank in the categories
-}
data MetaCategory = MetaCategory
  { getMetaCategoryName :: Name
  , getCategories :: [Category]
  } deriving (Show, Read, Eq)
derivePersistField "MetaCategory"

data CompetitionMeta = CompetitionMeta
  { getMetaName :: Name
  , getMetaDescription :: Description
  } deriving (Eq, Ord, Read, Show)

data Competition = Competition
  { getMetaData :: CompetitionMeta
  , getMetaCategories :: [MetaCategory]
  } deriving (Show, Read, Eq)
derivePersistField "Competition"

getCompetitionName :: Competition -> Name
getCompetitionName = getMetaName . getMetaData

getCompetitionDescription :: Competition -> Description
getCompetitionDescription = getMetaDescription . getMetaData

instance PathPiece Competition where
  toPathPiece comp = T.pack $ show comp
  fromPathPiece t = case reads (T.unpack t) of
    [(c, "")] -> return c
    _ -> Nothing
