module StarExec.Persist where

import Import
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import StarExec.Types
import Codec.Compression.GZip
import Data.Time.Clock
import qualified Data.List as L
import Data.Time.Clock
import Control.Concurrent.SSem
import Control.Monad.CatchIO ( bracket_ )

insertJobInfo :: JobInfo -> Handler ()
insertJobInfo jobInfo = runDB $ insert_ jobInfo

getPersistJobInfo :: Int -> Handler (Maybe JobInfo)
getPersistJobInfo _jobId = getEntity $ UniqueJobInfo _jobId

getPersistPostProcInfo :: Int -> Handler (Maybe PostProcInfo)
getPersistPostProcInfo _procId = getEntity $ UniquePostProcInfo _procId

getPersistJobResults :: Int -> Handler [JobResultInfo]
getPersistJobResults _jobId = getEntityList [ JobResultInfoJobId ==. _jobId ] []

getPersistJobResult :: Int -> Handler (Maybe JobResultInfo)
getPersistJobResult _pairId = getEntity $ UniqueJobResultInfo _pairId

getPersistCompetitions :: Handler [Entity CompetitionInfo]
getPersistCompetitions = runDB $ selectList [] [ Desc CompetitionInfoDate ]

getPersistPublicCompetitions :: Handler [Entity CompetitionInfo]
getPersistPublicCompetitions = runDB $ selectList [ CompetitionInfoPublic ==. True ] [ Desc CompetitionInfoDate ]

--getEntityList :: (YesodPersist site,
--                  PersistQuery (YesodPersistBackend site Handler),
--                  PersistEntity b,
--                  PersistMonadBackend (YesodPersistBackend site Handler)
--                  ~ PersistEntityBackend b) =>
--                       [Filter b] -> [SelectOpt b] -> Handler [b]
getEntityList _filter _opts = runDB $ do
  results <- selectList _filter _opts
  return $ map entityVal results

getEntity uniqueVal = runDB $ do
  mVal <- getBy uniqueVal
  case mVal of
    Nothing -> return Nothing
    Just val -> return $ Just $ entityVal val

getPersistJobPair :: Int -> Handler (Maybe JobPairInfo)
getPersistJobPair _pairId = getEntity $ UniqueJobPairInfo _pairId

getPersistSolverInfo :: Int -> Handler (Maybe SolverInfo)
getPersistSolverInfo _solverId = getEntity $ UniqueSolverInfo _solverId

getPersistBenchmarkInfo :: Int -> Handler (Maybe BenchmarkInfo)
getPersistBenchmarkInfo _benchId = getEntity $ UniqueBenchmarkInfo _benchId

dbInsertJobResult :: JobResultInfo -> Handler ()
dbInsertJobResult jobResult = runDB $ do
  _ <- insertUnique $ jobResult
  return ()

decompressText :: BS.ByteString -> Text
decompressText = TL.toStrict . decodeUtf8 . decompress . BSL.fromStrict

compressBS :: BS.ByteString -> BS.ByteString
compressBS = BSL.toStrict . compress . BSL.fromStrict

registerJobs :: [Int] -> Handler ()
registerJobs ids = do
  now <- liftIO getCurrentTime
  mapM_ (insertJob now) ids
  where
    insertJob now _id = do
      let j = JobInfo _id "" Started "" "" "" False True now Nothing now
      runDB $ insertUnique j

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

-- FIXME: this should use some form of bracketing
runDB_exclusive query = do
    app <- getYesod
    lift $ Control.Concurrent.SSem.wait   $ dbSem app
    out <- runDB query 
    lift $ Control.Concurrent.SSem.signal $ dbSem app


