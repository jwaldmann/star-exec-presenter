module Presenter.Model.Query where

import Yesod
import Prelude
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
