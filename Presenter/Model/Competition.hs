module Presenter.Model.Competition where

import Yesod
import Presenter.Model.Types
import Presenter.Model.StarExec
import Prelude
import qualified Data.Text as T

{-
  solver sorted by YES/CERTIFIED/NO, maybe with scoring -> SolverResult
-}
data Category = Category
  { getCategoryName :: Name
  , getCategoryScoring :: Scoring
  , getPostProcId :: PostProcId
  , getJobIds :: [Int]
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
