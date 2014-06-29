module StarExec.PersistTypes where

import Import
import Model
import qualified Data.Csv as CSV
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import Control.Applicative
import StarExec.Types

instance CSV.FromNamedRecord JobResultInfo where
  parseNamedRecord r =
    JobResultInfo (-1)
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
                | r == "yes"        = pure YES
                | r == "no"         = pure NO
                | r == "maybe"      = pure MAYBE
                | r == "certified"  = pure CERTIFIED
                | r == "error"      = pure ERROR
                | otherwise         = pure OTHER

{-
-}
data PrimInfo = PrimJobInfo JobInfo
                | PrimSolverInfo SolverInfo
                | PrimBenchmarkInfo BenchmarkInfo
                -- | PrimUserInfo UserInfo
                | PrimSpaceInfo SpaceInfo
  deriving (Show)

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

--toUserInfo :: PrimInfo -> UserInfo
--toUserInfo (PrimUserInfo info) = info

--toUserInfos :: [PrimInfo] -> [UserInfo]
--toUserInfos = map toUserInfo

toSpaceInfo :: PrimInfo -> SpaceInfo
toSpaceInfo (PrimSpaceInfo info) = info

toSpaceInfos :: [PrimInfo] -> [SpaceInfo]
toSpaceInfos = map toSpaceInfo

primInfoId :: PrimInfo -> Int
primInfoId primInfo =
  case primInfo of
    PrimJobInfo info       -> jobInfoStarExecId info
    PrimBenchmarkInfo info -> benchmarkInfoStarExecId info
    PrimSolverInfo info    -> solverInfoStarExecId info
    PrimSpaceInfo info     -> spaceId info
    --PrimUserInfo info      -> userId info

primInfoName :: PrimInfo -> Text
primInfoName primInfo =
  case primInfo of
    PrimJobInfo info       -> jobInfoName info
    PrimBenchmarkInfo info -> benchmarkInfoName info
    PrimSolverInfo info    -> solverInfoName info
    PrimSpaceInfo info     -> spaceName info
    --PrimUserInfo info      -> userName info

data QueryIntermediateResult =
  QIRJobInfo (Maybe JobInfo)
  | QIRSolverInfo (Maybe SolverInfo)
  | QIRBenchmarkInfo (Maybe BenchmarkInfo)
  | QIRJobPairInfo (Maybe JobPairInfo)
  | QIRJobResults [JobResultInfo]
  deriving (Show)
