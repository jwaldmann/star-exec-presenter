module Table.Data where

import qualified Data.Text as T

data Table = 
     Table { header :: [ Cell ]
           , rows :: [ Row ] }
type Row = [ Cell ]
data Cell =
     Cell { contents :: T.Text -- this is shown
          , url :: T.Text -- FIXME replace with proper type
          , tag :: T.Text -- used for sorting (e.g., YES, NO, CERTIFIED)
          }

