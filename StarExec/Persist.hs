module StarExec.Persist where

import Import
import StarExec.Types
import StarExec.Commands

getResultsFromStarExec :: ( MonadHandler m, MonadBaseControl IO m )
  => Int -> m [JobResultInfo]
getResultsFromStarExec _jobId = do
  con <- getConnection
  mInfo <- getJobInfo con _jobId
  return $ case mInfo of
    Just info -> info
    Nothing   -> []

fromPersistJobResultInfos :: [PersistJobResultInfo] -> [JobResultInfo]
fromPersistJobResultInfos = map fromPersistJobResultInfo

fromPersistJobResultInfo :: PersistJobResultInfo -> JobResultInfo
fromPersistJobResultInfo p = JobResultInfo
  { jriPairId = persistJobResultInfoPairId p
  , jriBenchmark = persistJobResultInfoBenchmark p
  , jriBenchmarkId = persistJobResultInfoBenchmarkId p
  , jriSolver = persistJobResultInfoSolver p
  , jriSolverId = persistJobResultInfoSolverId p
  , jriConfiguration = persistJobResultInfoConfiguration p
  , jriConfigurationId = persistJobResultInfoConfigurationId p
  , jriStatus = persistJobResultInfoStatus p
  , jriCpuTime = persistJobResultInfoCpuTime p
  , jriWallclockTime = persistJobResultInfoWallclockTime p
  , jriResult = persistJobResultInfoResult p
  }

dbInsertJobResult _jobId jobResult = do
  insertUnique $ PersistJobResultInfo
    _jobId
    (jriPairId jobResult)
    (jriBenchmark jobResult)
    (jriBenchmarkId jobResult)
    (jriSolver jobResult)
    (jriSolverId jobResult)
    (jriConfiguration jobResult)
    (jriConfigurationId jobResult)
    (jriStatus jobResult)
    (jriCpuTime jobResult)
    (jriWallclockTime jobResult)
    (jriResult jobResult)

getJobResults _jobId = do
  pResults <- runDB $ selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []
  if null pResults
    then do
      jobInfos <- getResultsFromStarExec _jobId
      pResults' <- runDB $ do
        _ <- mapM (dbInsertJobResult _jobId) jobInfos
        selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []
      --liftIO $ print $ show pResults
      return $ map entityVal pResults'
      --return pResults'
    else do
      return $ map entityVal pResults
