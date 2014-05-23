{-# LANGUAGE DeriveGeneric #-}

module StarExec.Types where

import Prelude
import Yesod
import Data.Text
import Data.Text.Encoding
import Database.Persist.TH
import Control.Applicative
import Text.Blaze
import Text.Blaze.Internal
import Control.Applicative
import Network.HTTP.Conduit
import GHC.Generics
import qualified Data.Csv as CSV

{-
-}
type Cookies = [Cookie]

{-
-}
type StarExecConnection = (Request, Manager)

{-
-}
data JobStatus = Complete | Incomplete
    deriving (Show, Read, Eq)
derivePersistField "JobStatus"

instance ToMarkup JobStatus where
    toMarkup = string . show
    preEscapedToMarkup = preEscapedString . show

{-
-}
data StarExecPrimType = Solver | Benchmark | Job | User | Space
    deriving (Eq, Show, Read)

{-
-}
data StarExecListType = Solvers | Benchmarks | Jobs | Users | Spaces
    deriving (Eq, Show, Read)

getColumns :: StarExecListType -> Int
getColumns Solvers = 2
getColumns Benchmarks = 2
getColumns Users = 3
getColumns Jobs = 6
getColumns Spaces = 2

{-
aaData: [[,…], [,…]]
iTotalDisplayRecords: 2
iTotalRecords: 2
sEcho: 1
-}
data ListPrimResult = ListPrimResult
  { aaData :: ![[Text]]
  , iTotalDisplayRecords :: Int
  , iTotalRecords :: Int
  , sEcho :: Int
  } deriving (Show, Generic)

instance FromJSON ListPrimResult

{-
-}
data PrimInfo = PrimJobInfo JobInfo
                | PrimSolverInfo SolverInfo
                | PrimBenchmarkInfo BenchmarkInfo
                | PrimUserInfo UserInfo
                | PrimSpaceInfo SpaceInfo

toJobInfo :: PrimInfo -> JobInfo
toJobInfo (PrimJobInfo info) = info

toSolverInfo :: PrimInfo -> SolverInfo
toSolverInfo (PrimSolverInfo info) = info

toBenchmarkInfo :: PrimInfo -> BenchmarkInfo
toBenchmarkInfo (PrimBenchmarkInfo info) = info

toUserInfo :: PrimInfo -> UserInfo
toUserInfo (PrimUserInfo info) = info

toSpaceInfo :: PrimInfo -> SpaceInfo
toSpaceInfo (PrimSpaceInfo info) = info

data JobInfo = JobInfo
  { jobId :: Int
  , jobName :: Text
  , jobStatus :: JobStatus
  , jobPairsCompleted :: Double
  , jobPairsNum :: Int
  , jobPairsFailed :: Double
  , jobDate :: Text
  }

data SolverInfo = SolverInfo
  { solverId :: Int
  , solverName :: Text
  , solverDescription :: Text
  }

data BenchmarkInfo = BenchmarkInfo
  { benchmarkId :: Int
  , benchmarkName :: Text
  , benchmarkType :: Text
  }

data UserInfo = UserInfo
  { userId :: Int
  , userName :: Text
  , userInstitution :: Text
  , userMail :: Text
  }

data SpaceInfo = SpaceInfo
  { spaceId :: Int
  , spaceName :: Text
  , spaceDescription :: Text
  }

{-
-}
data SolverResult = YES | NO | CERTIFIED | MAYBE | ERROR | OTHER Text
    deriving (Show, Read, Eq)
derivePersistField "SolverResult"

instance CSV.FromField SolverResult where
    parseField result = parseResult s
        where
            s = toLower $ decodeUtf8 result
            parseResult r
                | r == "yes"        = pure YES
                | r == "no"         = pure NO
                | r == "maybe"      = pure MAYBE
                | r == "certified"  = pure CERTIFIED
                | r == "error"      = pure ERROR
                | otherwise         = pure $ OTHER $ decodeUtf8 result

instance ToMarkup SolverResult where
    toMarkup = string . show
    preEscapedToMarkup = preEscapedString . show

{-
-}
data JobResultInfo = JobResultInfo
  { jriPairId :: Int
  , jriBenchmark :: Text
  , jriBenchmarkId :: Int
  , jriSolver :: Text
  , jriSolverId :: Int
  , jriConfiguration :: Text
  , jriConfigurationId :: Int
  , jriStatus :: Text
  , jriCpuTime :: Double
  , jriWallclockTime :: Double
  , jriResult :: SolverResult
  } deriving (Show, Read)

instance CSV.FromNamedRecord JobResultInfo where
  parseNamedRecord r =
    JobResultInfo <$> r CSV..: "pair id"
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

{-
-}
data ErrorID = Login | Unkown
    deriving (Eq, Show, Read)

instance PathPiece ErrorID where
    toPathPiece = toPathPiece . show
    fromPathPiece e = do
        err <- fromPathPiece e
        return $ read err
