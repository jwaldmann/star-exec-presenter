module Presenter.Short where
import Data.Text
-- | like Show, but with much more terse output
-- (e.g., so that it fits in a cell of a table)
class Short a where short :: a -> Text
