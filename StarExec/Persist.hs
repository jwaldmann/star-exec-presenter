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
getPersistJobInfo _jobId = getEntity $ UniqueJobInfo _jobId

getPersistJobResults :: Int -> Handler [JobResultInfo]
getPersistJobResults _jobId = getEntityList [ JobResultInfoJobId ==. _jobId ] []

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

getPersistBenchmarInfo :: Int -> Handler (Maybe BenchmarkInfo)
getPersistBenchmarInfo _benchId = getEntity $ UniqueBenchmarkInfo _benchId

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
