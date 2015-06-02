module Presenter.Model.PersistInstances where

import Model
import Prelude
import qualified Data.Csv as CSV
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Applicative
import Presenter.Model.StarExec
import qualified Presenter.Model.Complexity as C

instance CSV.FromNamedRecord JobResultInfo where
  parseNamedRecord r =
    JobResultInfo (-1) Nothing
                  <$> r CSV..: "pair id"
                  <*> r CSV..: "benchmark"
                  <*> r CSV..: "benchmark id"
                  <*> r CSV..: "solver"
                  <*> r CSV..: "solver id"
                  <*> r CSV..: "configuration"
                  <*> r CSV..: "configuration id"
                  <*> r CSV..: "status"
                  <*> r CSV..: "cpu time"
                  <*> r CSV..: "wallclock time"
                  <*> r CSV..: "result"

instance CSV.FromField SolverResult where
  parseField result = parseResult s
    where
      s = T.toLower $ decodeUtf8 result
      parseResult r
        | r == "no"         = pure NO
        | r == "maybe"      = pure MAYBE
        | r == "certified"  = pure CERTIFIED
        | r == "error"      = pure ERROR
        | r == "yes"        = pure $ YES Nothing
        | otherwise = case readsPrec 0 $ T.unpack $ decodeUtf8 result of
          [ ( C.Bounds { C.upper = C.Poly (Just deg) } , "" ) ]
            -> pure $ YES $ Just deg
          _ -> pure OTHER

instance CSV.FromField JobResultStatus where
  parseField result = parseResult s
    where
      s = T.toLower $ decodeUtf8 result
      parseResult r
        | r == "running"             = pure JobResultRunning
        | r == "enqueued"            = pure JobResultEnqueued
        | "pending" `T.isPrefixOf` r = pure JobResultPending
        | otherwise                  = pure JobResultComplete
