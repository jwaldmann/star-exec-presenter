{-# LANGUAGE DeriveGeneric #-}

module StarExec.Types where

import Prelude
import Yesod
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe
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
--data StarExecPrimType = Solver | Benchmark | Job | User | Space
--    deriving (Eq, Show, Read)

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
  { aaData :: ![[T.Text]]
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
  deriving (Show, Eq)

toJobInfo :: PrimInfo -> JobInfo
toJobInfo (PrimJobInfo info) = info

toJobInfos :: [PrimInfo] -> [JobInfo]
toJobInfos = map toJobInfo

toSolverInfo :: PrimInfo -> SolverInfo
toSolverInfo (PrimSolverInfo info) = info

toSolverInfos :: [PrimInfo] -> [SolverInfo]
toSolverInfos = map toSolverInfo

toBenchmarkInfo :: PrimInfo -> BenchmarkInfo
toBenchmarkInfo (PrimBenchmarkInfo info) = info

toBenchmarkInfos :: [PrimInfo] -> [BenchmarkInfo]
toBenchmarkInfos = map toBenchmarkInfo

toUserInfo :: PrimInfo -> UserInfo
toUserInfo (PrimUserInfo info) = info

toUserInfos :: [PrimInfo] -> [UserInfo]
toUserInfos = map toUserInfo

toSpaceInfo :: PrimInfo -> SpaceInfo
toSpaceInfo (PrimSpaceInfo info) = info

toSpaceInfos :: [PrimInfo] -> [SpaceInfo]
toSpaceInfos = map toSpaceInfo

data JobInfo = JobInfo
  { jobId :: Int
  , jobSpaceId :: Maybe Int
  , jobName :: T.Text
  , jobStatus :: JobStatus
  , jobPairsCompleted :: Double
  , jobPairsNum :: Int
  , jobPairsFailed :: Double
  , jobDate :: T.Text
  } deriving (Show, Eq)

data SolverInfo = SolverInfo
  { solverId :: Int
  , solverName :: T.Text
  , solverDescription :: T.Text
  } deriving (Show, Eq)

data BenchmarkInfo = BenchmarkInfo
  { benchmarkId :: Int
  , benchmarkName :: T.Text
  , benchmarkType :: T.Text
  } deriving (Show, Eq)

data UserInfo = UserInfo
  { userId :: Int
  , userName :: T.Text
  , userInstitution :: T.Text
  , userMail :: T.Text
  } deriving (Show, Eq)

data SpaceInfo = SpaceInfo
  { spaceId :: Int
  , spaceParentId :: Maybe Int
  , spaceName :: T.Text
  , spaceDescription :: T.Text
  } deriving (Show, Eq)

primInfoId :: PrimInfo -> Int
primInfoId primInfo =
  case primInfo of
    PrimJobInfo info       -> jobId info
    PrimSpaceInfo info     -> spaceId info
    PrimBenchmarkInfo info -> benchmarkId info
    PrimSolverInfo info    -> solverId info
    PrimSpaceInfo info     -> spaceId info
    PrimUserInfo info      -> userId info

primInfoName :: PrimInfo -> T.Text
primInfoName primInfo =
  case primInfo of
    PrimJobInfo info       -> jobName info
    PrimSpaceInfo info     -> spaceName info
    PrimBenchmarkInfo info -> benchmarkName info
    PrimSolverInfo info    -> solverName info
    PrimSpaceInfo info     -> spaceName info
    PrimUserInfo info      -> userName info

{-
-}
data SolverResult = YES | NO | CERTIFIED | MAYBE | ERROR | OTHER
    deriving (Show, Read, Eq)
derivePersistField "SolverResult"

instance CSV.FromField SolverResult where
    parseField result = parseResult s
        where
            s = T.toLower $ decodeUtf8 result
            parseResult r
                | r == "yes"        = pure YES
                | r == "no"         = pure NO
                | r == "maybe"      = pure MAYBE
                | r == "certified"  = pure CERTIFIED
                | r == "error"      = pure ERROR
                | otherwise         = pure OTHER

instance ToMarkup SolverResult where
    toMarkup = string . show
    preEscapedToMarkup = preEscapedString . show

{-
-}
data JobResultInfo = JobResultInfo
  { jriPairId :: Int
  , jriBenchmark :: T.Text
  , jriBenchmarkId :: Int
  , jriSolver :: T.Text
  , jriSolverId :: Int
  , jriConfiguration :: T.Text
  , jriConfigurationId :: Int
  , jriStatus :: T.Text
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
data JobPairInfo = JobPairInfo
  { jpiPairId :: Int
  , jpiStdout :: T.Text
  , jpiLog :: T.Text
  } deriving (Show, Read)

{-
-}
data ErrorID = Login | Unkown
    deriving (Eq, Show, Read)

instance PathPiece ErrorID where
    toPathPiece = toPathPiece . show
    fromPathPiece e = do
        err <- fromPathPiece e
        return $ read err
