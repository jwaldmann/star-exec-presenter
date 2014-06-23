module StarExec.Persist where

import Import
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import StarExec.Types
import Codec.Compression.GZip

insertJobInfo :: JobInfo -> Handler ()
insertJobInfo jobInfo = runDB $ insert_ jobInfo

getPersistJobInfo :: Int -> Handler (Maybe JobInfo)
getPersistJobInfo _jobId = runDB $ do
  pEntity <- getBy $ UniqueJobInfo _jobId
  case pEntity of
    Nothing -> return Nothing
    Just en -> return $ Just $ entityVal en

--fromPersistJobResultInfos :: [PersistJobResultInfo] -> [JobResultInfo]
--fromPersistJobResultInfos = map fromPersistJobResultInfo

--fromPersistJobResultInfo :: PersistJobResultInfo -> JobResultInfo
--fromPersistJobResultInfo p = JobResultInfo
--  { jriPairId = persistJobResultInfoPairId p
--  , jriBenchmark = persistJobResultInfoBenchmark p
--  , jriBenchmarkId = persistJobResultInfoBenchmarkId p
--  , jriSolver = persistJobResultInfoSolver p
--  , jriSolverId = persistJobResultInfoSolverId p
--  , jriConfiguration = persistJobResultInfoConfiguration p
--  , jriConfigurationId = persistJobResultInfoConfigurationId p
--  , jriStatus = persistJobResultInfoStatus p
--  , jriCpuTime = persistJobResultInfoCpuTime p
--  , jriWallclockTime = persistJobResultInfoWallclockTime p
--  , jriResult = persistJobResultInfoResult p
--  }

--toPersistJobResultInfo :: Int -> JobResultInfo -> PersistJobResultInfo
--toPersistJobResultInfo _jobId j = PersistJobResultInfo
--  _jobId
--  (jriPairId j)
--  (jriBenchmark j)
--  (jriBenchmarkId j)
--  (jriSolver j)
--  (jriSolverId j)
--  (jriConfiguration j)
--  (jriConfigurationId j)
--  (jriStatus j)
--  (jriCpuTime j)
--  (jriWallclockTime j)
--  (jriResult j)

--fromPersistJobPairInfo :: PersistJobPairInfo -> JobPairInfo
--fromPersistJobPairInfo p = JobPairInfo
--  { jpiPairId = persistJobPairInfoPairId p
--  , jpiStdout = decompressText $ persistJobPairInfoStdout p
--  , jpiLog = decompressText $ persistJobPairInfoLog p
--  }

dbInsertJobResult :: JobResultInfo -> Handler ()
dbInsertJobResult jobResult = runDB $ do
  _ <- insertUnique $ jobResult
  return ()

decompressText :: BS.ByteString -> Text
decompressText = TL.toStrict . decodeUtf8 . decompress . BSL.fromStrict

compressBS :: BS.ByteString -> BS.ByteString
compressBS = BSL.toStrict . compress . BSL.fromStrict
