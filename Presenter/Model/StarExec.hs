{-# language StandaloneDeriving #-}

module Presenter.Model.StarExec where

import Presenter.Model.Types
import Prelude
import Yesod
import Text.Blaze
import Text.Blaze.Internal
import Data.Text (Text)
import qualified Data.Text as T

import qualified Presenter.Model.Complexity2015 as C

data JobStatus =
  Complete
  | Incomplete
  | Started
  deriving (Show, Read, Eq, Ord)
derivePersistField "JobStatus"

instance ToMarkup JobStatus where
  toMarkup = string . show
  preEscapedToMarkup = preEscapedString . show

data SolverResult =
    YES 
  | NO
  | BOUNDS C.Bounds
  | CERTIFIED
  | MAYBE
  | ERROR
  | OTHER Text
  deriving (Show, Read, Eq, Ord)
derivePersistField "SolverResult"

data JobResultStatus =
  JobResultComplete
  | JobResultRunning
  | JobResultEnqueued
  | JobResultPending
  | JobResultUndetermined
  deriving (Show, Read, Eq, Ord)
derivePersistField "JobResultStatus"

data StarExecSpace = StarExecSpace
  { spaceId :: Int
  , spaceParentId :: Maybe Int
  , spaceName :: Name
  , spaceDescription :: Description
  } deriving (Show, Eq)

-- | this is (some of) the data in the xml file returned by download-space-XML
-- (for the moment, only benchmarks, ignoring permissions and solvers)
data Space = Space 
  { spId :: Int
  , spName :: Name
  , children :: [Space]
  , benchmarks :: [ Int ]
  } deriving ( Show, Eq )

families :: Space -> [ (Name, [Int]) ]
families s = 
  let path ns = T.intercalate "/" ns
      walk s' = 
        let here = benchmarks s'
            below = children s' >>= walk 
        in    map ( \ (p,b) -> (spName s' : p, b) )
            $ if Prelude.null here then below else ([], here) : below
  in  map ( \ (p,b) -> (path p, b) ) $ walk s

all_in_hierarchy :: Space -> [Int]
all_in_hierarchy s =
  benchmarks s ++ (children s >>= all_in_hierarchy)

data StarExecJob = SEJob
  { postproc_id :: Int
  , description :: Text
  , job_name :: Text
  , queue_id :: Int
  , mem_limit :: Double
  , wallclock_timeout :: Int
  , cpu_timeout :: Int
  , start_paused :: Bool
  , jobpairs :: [ StarExecJobPair ] -- ^ jobspace, benchmark, config
  , jobid :: Maybe Int
  } deriving ( Show )

data StarExecJobPair = SEJobPair
  { jobPairSpace :: Text   
  , jobPairBench :: Int
  , jobPairConfig :: Int 
  } deriving ( Show )


data QueryStatus k = Pending (Key k) | Latest

instance Show (QueryStatus k) where
  show qs = case qs of
    Pending _ -> "Pending _"
    Latest -> "Latest"

data QueryResult k a = QueryResult
  { queryStatus :: QueryStatus k
  , queryResult :: a
  }
  deriving (Show)

data SEQuery =
  GetJobInfo Int
  | GetSolverInfo Int
  | GetBenchmarkInfo Int
  | GetJobPair Int
  | GetJobResults Int
  | GetJob Int
  | GetPostProc Int
  deriving (Eq, Read, Show)
derivePersistField "SEQuery"
