module StarExec.Persist where

import Import
import Control.Monad.IO.Class
import Data.Time.Clock
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TL
import StarExec.Types
import StarExec.Connection
import StarExec.Commands
import Codec.Compression.GZip

insertJobInfo jobInfo = runDB $ do
  let _id = jobId jobInfo
      _name = jobName jobInfo
      _status = jobStatus jobInfo
      _date = jobDate jobInfo
      pJobInfo = PersistJobInfo _id _name _status _date
  insert_ pJobInfo

getPersistJobInfo _jobId = runDB $ do
  pEntity <- getBy $ UniquePersistJobInfo _jobId
  case pEntity of
    Nothing -> return Nothing
    Just en -> return $ Just $ entityVal en

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

toPersistJobResultInfo :: Int -> JobResultInfo -> PersistJobResultInfo
toPersistJobResultInfo _jobId j = PersistJobResultInfo
  _jobId
  (jriPairId j)
  (jriBenchmark j)
  (jriBenchmarkId j)
  (jriSolver j)
  (jriSolverId j)
  (jriConfiguration j)
  (jriConfigurationId j)
  (jriStatus j)
  (jriCpuTime j)
  (jriWallclockTime j)
  (jriResult j)

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

decompressText :: BS.ByteString -> Text
decompressText = TL.toStrict . decodeUtf8 . decompress . BSL.fromStrict

compressText :: Text -> BS.ByteString
compressText = BSL.toStrict . compress . encodeUtf8 . TL.fromStrict
