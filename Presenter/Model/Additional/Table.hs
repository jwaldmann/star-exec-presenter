module Presenter.Model.Additional.Table where

import Presenter.Model.Entities
import Presenter.Model.Query
import Presenter.Model.RouteTypes (JobID)
import Prelude (Maybe)
import Foundation (Widget)

import qualified Data.Text as T
import qualified Data.List
import qualified Data.Map.Strict as M
import qualified Prelude as P

data Table =
     Table { header :: [ Cell ]
           , rows :: [ Row ]
           }

type Row = [ Cell ]

type Col = [ Cell ]

cols :: Table -> [ Col ] 
cols t = Data.List.transpose (rows t)

data Cell =
     Cell { contents :: ! Widget -- ^ this is shown
          , tdclass :: ! T.Text
          , tag :: ! T.Text -- ^ used for sorting (e.g., YES, NO, CERTIFIED)
          , nums :: ! (M.Map Numtag P.Double)
          , mbench :: ! (Maybe T.Text) -- ^  if this is cell for a benchmark
          , msolver :: ! (Maybe T.Text) -- ^ who made this?
          , mjr :: ! (Maybe JobResult)
          , mjid :: ! (Maybe JobID)
          , url :: ! T.Text -- ^ FIXME replace with proper type
          }

