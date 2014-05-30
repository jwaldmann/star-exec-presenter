module StarExec.Persist where

import Import
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import StarExec.Types
import StarExec.Connection
import StarExec.Commands
import Codec.Compression.GZip

getJobResultsFromStarExec :: ( MonadHandler m, MonadBaseControl IO m )
  => Int -> m [JobResultInfo]
getJobResultsFromStarExec _jobId = do
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

fromPersistJobPairInfo :: PersistJobPairInfo -> JobPairInfo
fromPersistJobPairInfo p = JobPairInfo
  { jpiPairId = persistJobPairInfoPairId p
  , jpiStdout = decompressText $ persistJobPairInfoStdout p
  , jpiLog = decompressText $ persistJobPairInfoLog p
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
      jobInfos <- getJobResultsFromStarExec _jobId
      pResults' <- runDB $ do
        _ <- mapM (dbInsertJobResult _jobId) jobInfos
        selectList [ PersistJobResultInfoStarExecJobId ==. _jobId ] []
      --liftIO $ print $ show pResults
      return $ map entityVal pResults'
      --return pResults'
    else do
      return $ map entityVal pResults

decompressText :: BS.ByteString -> Text
decompressText = TL.toStrict . decodeUtf8 . decompress . BSL.fromStrict

compressText :: Text -> BS.ByteString
compressText = BSL.toStrict . compress . encodeUtf8 . TL.fromStrict

getJobPair _pairId = runDB $ do
  mPair <- getBy $ UniquePersistJobPairInfo _pairId
  case mPair of
    Just pair -> return $ Just $ fromPersistJobPairInfo $ entityVal pair
    Nothing -> do
      con <- getConnection
      mPairInfo <- getJobPairInfo con _pairId
      case mPairInfo of
        Just pairInfo -> do
          mKey <- insertUnique $ PersistJobPairInfo
                                  _pairId
                                  (compressText $ jpiStdout pairInfo)
                                  (compressText $ jpiLog pairInfo)
          case mKey of
            Just key -> do
              mVal <- get key
              case mVal of
                Just val -> return $ Just $ fromPersistJobPairInfo val
                Nothing -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
