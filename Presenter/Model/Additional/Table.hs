module Presenter.Model.Additional.Table where

import Presenter.Model.Entities
import Presenter.Model.RouteTypes (JobID)
import Prelude (Maybe)
import Foundation (Widget)

import qualified Data.Text as T

data Table =
     Table { header :: [ Cell ]
           , rows :: [ Row ]
           }

type Row = [ Cell ]

data Cell =
     Cell { contents :: Widget -- ^ this is shown
          , tdclass :: T.Text
          , tag :: T.Text -- ^ used for sorting (e.g., YES, NO, CERTIFIED)
          , msolver :: Maybe T.Text -- ^ who made this?
          , mjr :: Maybe JobResult
          , mjid :: Maybe JobID
          , url :: T.Text -- ^ FIXME replace with proper type
          }
