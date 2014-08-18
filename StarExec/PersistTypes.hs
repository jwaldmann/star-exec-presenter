module StarExec.PersistTypes where

import Import
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Read as TRead
import Data.Text.Encoding
import GHC.Generics
import Control.Applicative
import StarExec.Types

import qualified StarExec.Complexity as C

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
                -- | otherwise       = pure $ JobResultOther s
