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

decompressText :: BS.ByteString -> Text
decompressText = TL.toStrict . decodeUtf8 . decompress . BSL.fromStrict

compressText :: Text -> BS.ByteString
compressText = BSL.toStrict . compress . encodeUtf8 . TL.fromStrict
