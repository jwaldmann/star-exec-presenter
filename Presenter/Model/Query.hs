module Presenter.Model.Query where

import Prelude
import Yesod (PathPiece(..))
import Presenter.Model.Types
import Presenter.Model.RouteTypes
import Presenter.Model.StarExec
import qualified Data.Text as T
import qualified Prelude as P

data Numtag = CPU | Wall | Size
  deriving (P.Eq, P.Ord, P.Show, P.Read, P.Enum, P.Bounded)

data Level = Min | Bot | Med | Top | Max | Sum
  deriving (P.Eq, P.Ord, P.Show, P.Read, P.Enum, P.Bounded)

levels :: [ Level ]
levels = [ P.minBound .. P.maxBound ]


instance PathPiece Query where
  fromPathPiece "noquery" = return NoQuery
  fromPathPiece t = case reads (T.unpack t) of
    [ (q, "") ] -> return q
    _ -> Nothing
  toPathPiece NoQuery = "noquery"
  toPathPiece q = T.pack $ show q

data Query =
  NoQuery
  | Query [ Transform ]
  deriving (Read,Show, Eq)

data Direction = Up | Down
  deriving (Read,Show, Eq)

data Transform =
  Choose_Columns [ Int ]
  | Filter_Rows Predicate
  | VBestAll | VBestInit
  | Sort Direction Int (Maybe Numtag) -- ^ no numtag: sort by result
  deriving (Read,Show, Eq)

data Predicate =
  And [ Cell_Filter ]
  | Not Predicate
  | Compare Int Numtag Ordering Double
  deriving (Read,Show, Eq)

data Cell_Filter =
  Any
  | Equals T.Text
  | Not_Equals T.Text
  deriving (Read,Show, Eq)

-- draft for a more generic querying

data NQuery =
  NNoQuery
  | NQuery [ NTransform ]
  deriving (Read,Show, Eq)

data NTransform =
  ColumnsT [ Int ]
  | SolversT [ Name ]
  | RowsT RowFilter
  deriving (Read,Show, Eq)

data RowFilter =
  RAnd [ CellFilter ]
  | RNot [ CellFilter ]
  deriving (Read,Show, Eq)

data CellFilter =
  CAny
  | CAnd SolverResult
  | COr SolverResult
  | CNot SolverResult
  -- maybe add CAndYes, COrYes and CNotYes
  -- for all YESes
  -- because "YES (Maybe Int)" is not expressive enough
  -- what does "YES Nothing" mean in this context?
  deriving (Read,Show, Eq)
