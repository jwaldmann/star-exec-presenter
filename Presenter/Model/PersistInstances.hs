module Presenter.Model.PersistInstances where

import Model
import Prelude
import qualified Data.Csv as CSV
import qualified Data.Text as T
import Data.Text.Encoding
import Presenter.Model.StarExec
import Presenter.Output

instance CSV.FromNamedRecord JobResultInfo where
  parseNamedRecord r =
    JobResultInfo (-1) Nothing
                  <$> r CSV..: "pair id"
                  <*> r CSV..: "benchmark"
                  <*> r CSV..: "benchmark id"
                  <*> pure Nothing
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
      s = decodeUtf8 result
      parseResult r
        | r == "NO"         = pure NO
        | r == "MAYBE"      = pure MAYBE
        | r == "CERTIFIED"  = pure CERTIFIED
        | r == "ERROR"      = pure ERROR
        | r == "YES"        = pure YES
        | otherwise = case readsPrec 0 $ T.unpack s of
          [ ( res , "" ) ] -> pure $ BOUNDS res
          _ -> pure $ OTHER s

instance CSV.FromField JobResultStatus where
  parseField result = parseResult s
    where
      s = T.toLower $ decodeUtf8 result
      parseResult r
        | r == "running"             = pure JobResultRunning
        | r == "enqueued"            = pure JobResultEnqueued
        | "pending" `T.isPrefixOf` r = pure JobResultPending
        | otherwise                  = pure JobResultComplete


instance Output CompetitionInfo where
    output c = "CompetitionInfo" <#> dutch_record
             [ "competitionInfoCompetition" <+> equals <#> output (competitionInfoCompetition c)
             , "competitionInfoDate" <+> equals <#> output (competitionInfoDate c)
             , "competitionInfoPublic" <+> equals <#> output (competitionInfoPublic c)
             ]
