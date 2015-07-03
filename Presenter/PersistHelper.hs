module Presenter.PersistHelper where

import Import
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import Codec.Compression.GZip
import Data.Time.Clock
import qualified Data.List as L
import Control.Concurrent.SSem

-- ###### persist getter ######

getEntityList _filter _opts = runDB $ getEntityList' _filter _opts

getEntityList' _filter _opts = do
  results <- selectList _filter _opts
  return $ map entityVal results

getEntity uniqueVal = runDB $ getEntity' uniqueVal

getEntity' uniqueVal = do
  mVal <- getBy uniqueVal
  return $ entityVal <$> mVal

getEntityVal' uniqueVal dataConstructor = do
  mVal <- getEntity' uniqueVal
  return $ dataConstructor <$> mVal

getPersistStarExecJobInfo :: Int -> Handler (Maybe JobInfo)
getPersistStarExecJobInfo = getEntity . UniqueJobInfo

getPersistStarExecJobInfo' :: Int -> YesodDB App (Maybe JobInfo)
getPersistStarExecJobInfo' = getEntity' . UniqueJobInfo

getPersistJobInfo :: JobID -> Handler (Maybe Job)
getPersistJobInfo = runDB . getPersistJobInfo'

getPersistJobInfo' :: JobID -> YesodDB App (Maybe Job)
getPersistJobInfo' (StarExecJobID _id) = getEntityVal' (UniqueJobInfo _id) StarExecJob
getPersistJobInfo' (LriJobID _id) = getEntityVal' (UniqueLriJobInfo _id) LriJob
getPersistJobInfo' (UibkJobID _id) = getEntityVal' (UniqueUibkJobInfo _id) UibkJob

getPersistPostProcInfo :: Int -> Handler (Maybe PostProcInfo)
getPersistPostProcInfo = getEntity . UniquePostProcInfo

getPersistPostProcInfo' :: Int -> YesodDB App (Maybe PostProcInfo)
getPersistPostProcInfo' = getEntity' . UniquePostProcInfo

getPersistJobResults :: JobID -> Handler [JobResult]
getPersistJobResults = runDB . getPersistJobResults'

getPersistJobResults' :: JobID -> YesodDB App [JobResult]
getPersistJobResults' (StarExecJobID _id) = do
  results <- getEntityList' [ JobResultInfoJobId ==. _id ] []
  return $ StarExecResult <$> results
getPersistJobResults' (LriJobID _id) = do
  results <- getEntityList' [ LriResultInfoJobId ==. _id ] []
  return $ LriResult <$> results
getPersistJobResults' (UibkJobID _id) = do
  results <- getEntityList' [ UibkResultInfoJobId ==. _id ] []
  return $ UibkResult <$> results

getPersistJobResult :: JobPairID -> Handler (Maybe JobResult)
getPersistJobResult = runDB . getPersistJobResult'

getPersistJobResult' :: JobPairID -> YesodDB App (Maybe JobResult)
getPersistJobResult' (StarExecPairID _id) = getEntityVal' (UniqueJobResultInfo _id) StarExecResult
getPersistJobResult' _ = error "getPersistJobResult': not yet implemented"

getPersistStarExecJobResults :: Int -> Handler [JobResultInfo]
getPersistStarExecJobResults _jobId = getEntityList [ JobResultInfoJobId ==. _jobId ] []

getPersistStarExecJobResults' :: Int -> YesodDB App [JobResultInfo]
getPersistStarExecJobResults' _jobId = getEntityList' [ JobResultInfoJobId ==. _jobId ] []

getPersistCompetitions :: Handler [Entity CompetitionInfo]
getPersistCompetitions = runDB $ getPersistCompetitions'

getPersistCompetitions' :: YesodDB App [Entity CompetitionInfo]
getPersistCompetitions' = selectList [] [ Desc CompetitionInfoDate ]

getPersistPublicCompetitions :: Handler [Entity CompetitionInfo]
getPersistPublicCompetitions = runDB $ getPersistPublicCompetitions'

getPersistPublicCompetitions' :: YesodDB App [Entity CompetitionInfo]
getPersistPublicCompetitions' = selectList [ CompetitionInfoPublic ==. True ] [ Desc CompetitionInfoDate ]

getPersistJobPair :: JobPairID -> Handler (Maybe Pair)
getPersistJobPair = runDB . getPersistJobPair'

getPersistJobPair' :: JobPairID -> YesodDB App (Maybe Pair)
getPersistJobPair' (StarExecPairID _id) = getEntityVal' (UniqueJobPairInfo _id) StarExecPair
getPersistJobPair' _ = error "getPersistJobPair': not yet implemented"

getPersistSolverInfo :: SolverID -> Handler (Maybe Solver)
getPersistSolverInfo = runDB . getPersistSolverInfo'

getPersistSolverInfo' :: SolverID -> YesodDB App (Maybe Solver)
getPersistSolverInfo' (StarExecSolverID _id) = getEntityVal' (UniqueSolverInfo _id) StarExecSolver
getPersistSolverInfo' (LriSolverID _id) = getEntityVal' (UniqueLriSolverInfo _id) LriSolver
getPersistSolverInfo' (UibkSolverID _id) = getEntityVal' (UniqueUibkSolverInfo _id) UibkSolver

getPersistBenchmarkInfo :: BenchmarkID -> Handler (Maybe Benchmark)
getPersistBenchmarkInfo = runDB . getPersistBenchmarkInfo'

getPersistBenchmarkInfo' :: BenchmarkID -> YesodDB App (Maybe Benchmark)
getPersistBenchmarkInfo' (StarExecBenchmarkID _id) = getEntityVal' (UniqueBenchmarkInfo _id) StarExecBenchmark
getPersistBenchmarkInfo' (LriBenchmarkID _id) = getEntityVal' (UniqueLriBenchmarkInfo _id) LriBenchmark
getPersistBenchmarkInfo' (UibkBenchmarkID _id) = getEntityVal' (UniqueUibkBenchmarkInfo _id) UibkBenchmark

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
  -- liftIO $ putStrLn "#### updateJobInfo: jobInfo"
  -- liftIO $ putStrLn $ show jobInfo
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
      _ <- insertUnique $ jobInfo { jobInfoLastUpdate = currentTime }
      return ()

updatePostProcInfo :: PostProcInfo -> YesodDB App ()
updatePostProcInfo pInfo = do
  let _id = postProcInfoStarExecId pInfo
  deleteWhere [ PostProcInfoStarExecId ==. _id ]
  _ <- insertUnique pInfo
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
