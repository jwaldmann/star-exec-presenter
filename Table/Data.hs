module Table.Data where

import Model
import Prelude (Maybe)
import Import (Widget)

import qualified Data.Text as T

data Table = 
     Table { header :: [ Cell ]
           , rows :: [ Row ] }
type Row = [ Cell ]
data Cell =
     Cell { contents :: Widget -- this is shown
          , tdclass :: T.Text
          , tag :: T.Text -- used for sorting (e.g., YES, NO, CERTIFIED)

          -- probably don't need these (since it's contained in contents)
          , mjri :: Maybe JobResultInfo
          , url :: T.Text -- FIXME replace with proper type
          }

