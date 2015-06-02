module Presenter.Model.Query where

import Yesod
import Prelude
import Presenter.Model.Types
import Presenter.Model.StarExec
import qualified Data.Text as T

data Query =
  NoQuery
  | Query [ Transform ]
  deriving (Read,Show, Eq)

data Transform =
  Choose_Columns [ Int ]
  | Filter_Rows Predicate 
  deriving (Read,Show, Eq)               

data Predicate =
  And [ Cell_Filter ]
  | Not Predicate
  deriving (Read,Show, Eq)               

data Cell_Filter =
  Any 
  | Equals T.Text 
  | Not_Equals T.Text 
  deriving (Read,Show, Eq)

-- draft for a more generic querrying

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
