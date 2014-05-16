module StarExec.JobInfo where

import qualified Data.Csv as CSV
import Control.Applicative
import Prelude
import Data.Text

data JobInfo = JobInfo
  { pairId :: Int
  , benchmark :: Text
  , benchmarkId :: Int
  , solver :: Text
  , solverId :: Int
  , configuration :: Text
  , configurationId :: Int
  , status :: Text
  , cpuTime :: Double
  , wallclockTime :: Double
  , result :: Text
  } deriving (Show, Read)

instance CSV.FromNamedRecord JobInfo where
  parseNamedRecord r =
    JobInfo <$> r CSV..: "pair id"
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
