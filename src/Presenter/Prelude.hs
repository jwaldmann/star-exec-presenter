module Presenter.Prelude 
  ( module Presenter.Prelude
  ) where

import Prelude
import Presenter.Model.Types (Seconds)
import Data.Time.Clock

fromDiffTime :: NominalDiffTime -> Seconds
fromDiffTime = fromRational . toRational

diffTime :: UTCTime -> UTCTime -> Seconds
diffTime t1 t2 = fromDiffTime $ diffUTCTime t1 t2
