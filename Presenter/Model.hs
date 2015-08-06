{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Presenter.Model
  ( module Presenter.Model.Competition
  , module Presenter.Model.CompetitionResults
  , module Presenter.Model.Complexity2015
  , module Presenter.Model.Defaults
  , module Presenter.Model.Entities
  , module Presenter.Model.PersistInstances
  , module Presenter.Model.Query
  , module Presenter.Model.RouteTypes
  , module Presenter.Model.StarExec
  , module Presenter.Model.Types
  , module Presenter.DOI.Type
  , module Model
  ) where

import Presenter.Model.Competition
import Presenter.Model.CompetitionResults
import Presenter.Model.Complexity2015
import Presenter.Model.Defaults
import Presenter.Model.Entities
import Presenter.Model.PersistInstances
import Presenter.Model.Query
import Presenter.Model.RouteTypes
import Presenter.Model.StarExec
import Presenter.Model.Types
import Presenter.DOI.Type
import Model
import Prelude

deriving instance (Eq (QueryStatus QueryInfo))
