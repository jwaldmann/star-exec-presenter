module Presenter.PersistHelper where

import Import
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import Codec.Compression.GZip
import Data.Time.Clock
import qualified Data.List as L
import Data.Time.Clock
import Control.Concurrent.SSem
import Control.Monad.CatchIO ( bracket_ )
import Control.Applicative

-- ###### persist getter ######

getPersistJobInfo :: JobID -> Handler (Maybe Job)
getPersistJobInfo (StarExecJobID _id) = undefined
getPersistJobInfo (LriJobID _id) = undefined

getPersistPostProcInfo :: Int -> Handler (Maybe PostProcInfo)
getPersistPostProcInfo _procId = undefined

getPersistJobResults :: JobID -> Handler [JobResult]
getPersistJobResults (StarExecJobID _id) = undefined
getPersistJobResults (LriJobID _id) = undefined

getPersistJobResult :: JobPairID -> Handler (Maybe JobResult)
getPersistJobResult (StarExecPairID _id) = undefined
getPersistJobResult (LriPairID _id) = undefined

getPersistCompetitions :: Handler [Entity CompetitionInfo]
getPersistCompetitions = undefined

getPersistPublicCompetitions :: Handler [Entity CompetitionInfo]
getPersistPublicCompetitions = undefined

getPersistJobPair :: JobPairID -> Handler (Maybe Pair)
getPersistJobPair = undefined

getPersistSolverInfo :: SolverID -> Handler (Maybe Solver)
getPersistSolverInfo = undefined

getPersistBenchmarkInfo :: BenchmarkID -> Handler (Maybe Benchmark)
getPersistBenchmarkInfo = undefined

-- ###### persist setter (inserts) ######

insertJobInfo :: JobInfo -> Handler ()
insertJobInfo jobInfo = runDB $ insert_ jobInfo

insertJobResultInfo :: JobResultInfo -> Handler ()
insertJobResultInfo resultInfo = runDB $ do
  _ <- insertUnique $ resultInfo
  return ()

registerJobs :: [Int] -> Handler ()
registerJobs ids = do
  now <- lift getCurrentTime
  mapM_ (insertJob now) ids
  where
    insertJob now _id = runDB $ insertUnique $ defaultJobInfo
      { jobInfoStarExecId = _id
      , jobInfoStartDate = now
      , jobInfoLastUpdate = now
      }

-- ###### persist updater ######

updateJobInfo :: (Maybe JobInfo) -> JobInfo -> YesodDB App ()
updateJobInfo mJobInfo jobInfo = do
  currentTime <- liftIO getCurrentTime
  case mJobInfo of
    Just ji -> updateWhere
      [ JobInfoStarExecId ==. jobInfoStarExecId ji ]
      [ JobInfoName =. jobInfoName jobInfo
      , JobInfoStatus =. jobInfoStatus jobInfo
      , JobInfoDate =. jobInfoDate jobInfo
      , JobInfoPreProc =. jobInfoPreProc jobInfo
      , JobInfoPostProc =. jobInfoPostProc jobInfo
      , JobInfoIsComplexity =. jobInfoIsComplexity jobInfo
      , JobInfoFinishDate =. case jobInfoFinishDate ji of
                              Nothing -> if jobInfoStatus jobInfo == Complete
                                          then Just currentTime
                                          else Nothing
                              Just fd -> Just fd
      , JobInfoLastUpdate =. currentTime
      ]
    Nothing -> do
      insertUnique $ jobInfo { jobInfoLastUpdate = currentTime }
      return ()

updatePostProcInfo :: PostProcInfo -> YesodDB App ()
updatePostProcInfo pInfo = do
  let _id = postProcInfoStarExecId pInfo
  deleteWhere [ PostProcInfoStarExecId ==. _id ]
  insertUnique pInfo
  return ()

updateJobResults :: [JobResultInfo] -> YesodDB App ()
updateJobResults results = do
  let jobIds = L.nub $ map jobResultInfoJobId results
      deleteFilter = map (\i -> [JobResultInfoJobId ==. i]) jobIds
  deleteWhere $ linkByOr deleteFilter
  mapM_ insertUnique results
  where
    linkByOr [] = []
    linkByOr [x] = x
    linkByOr xs = L.foldr1 (||.) xs

-- ###### Helper ######

decompressText :: BS.ByteString -> Text
decompressText = TL.toStrict . decodeUtf8 . decompress . BSL.fromStrict

compressBS :: BS.ByteString -> BS.ByteString
compressBS = BSL.toStrict . compress . BSL.fromStrict

-- FIXME: this should use some form of bracketing
runDB_exclusive :: YesodDB App b -> Handler b
runDB_exclusive query = do
    app <- getYesod
    lift $ Control.Concurrent.SSem.wait   $ dbSem app
    lift $ putStrLn "Control.Concurrent.SSem.wait ...."
    out <- runDB query 
    lift $ Control.Concurrent.SSem.signal $ dbSem app
    lift $ putStrLn "... Control.Concurrent.SSem.signal"
    return out
