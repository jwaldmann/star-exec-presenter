{-
  StarExecCommands: Module that contains and handles all request to the
  starexec-cluster
-}

module StarExec.Commands
  ( getJobResults
  , getJobPairInfo
  , getJobInfo
  ) where

import Import
import Prelude (head)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import StarExec.Types
import StarExec.Urls
import StarExec.Connection
import qualified Codec.Archive.Zip as Zip
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import Text.HTML.DOM
import Text.XML.Cursor

-- internal Methods

decodeUtf8Body :: Response BSL.ByteString -> Text
decodeUtf8Body = TE.decodeUtf8 . BSL.toStrict . responseBody

cursorFromDOM :: BSL.ByteString -> Cursor
cursorFromDOM = fromDocument . parseLBS

getJobTitle :: Cursor -> Text
getJobTitle c = head $ content h1
  where h1 = head $ descendant c >>= element "h1" >>= child

getJobInfoFieldset :: Cursor -> Cursor
getJobInfoFieldset c = head $ descendant c >>= element "fieldset" >>= attributeIs "id" "detailField"

constructJobInfo :: Int -> Text -> [Cursor] -> JobInfo
constructJobInfo _jobId title tds =
  let baseJobInfo = JobInfo { jobId = _jobId
                            , jobName = title
                            , jobStatus = Incomplete
                            , jobDate = "" }
      getJobStatus t = case t of
                        "complete" -> Complete
                        _ -> Incomplete
      parseTDs info xs =
        case xs of
          ("status":t:tds) -> parseTDs
                                (info { jobStatus = getJobStatus t })
                                tds
          ("created":t:tds) -> parseTDs
                                  (info { jobDate = t })
                                  tds
          (_:tds) -> parseTDs info tds
          _ -> info
      safeHead list =
        case list of
          (_:_) -> head list
          _ -> ""
      tds' = map (safeHead . content) tds
      jobInfo = parseTDs baseJobInfo tds'
  in jobInfo

-- API

getJobInfo :: ( MonadHandler m ) =>
  StarExecConnection -> Int -> m (Maybe JobInfo)
getJobInfo (sec, man, cookies) _jobId = do
  let (+>) = BS.append
      req = sec { method = "GET"
                , path = jobInfoPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                }
  resp <- sendRequest (req, man, cookies)
  let cursor = cursorFromDOM $ responseBody resp
      jobTitle = getJobTitle cursor
  if "http" == T.take 4 jobTitle
    then return Nothing
    else do
      let fieldset = getJobInfoFieldset cursor
          tds = descendant fieldset >>= element "td" >>= child
      return $ Just $ constructJobInfo _jobId jobTitle tds
  --liftIO $ putStrLn $ show $ constructJobInfo _jobId jobTitle tds
  --return Nothing

getJobResults :: ( MonadHandler m ) =>
 StarExecConnection -> Int -> m (Maybe [JobResultInfo])
getJobResults (sec, man, cookies) _jobId = do
  let (+>) = BS.append
      req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                                +> "&type=job&returnids=true"
                }
  resp <- sendRequest (req, man, cookies)
  let archive = Zip.toArchive $ responseBody resp
  jobs <- case Zip.zEntries archive of
            entry:_ -> do
              let eitherVector = CSV.decodeByName $ Zip.fromEntry entry
              case eitherVector of
                Left msg -> do
                  liftIO $ putStrLn msg
                  return Nothing
                Right (_, jobInfos) ->
                  return $ Just $ Vector.toList jobInfos
            [] -> return Nothing
  return jobs

getJobPairInfo :: ( MonadHandler m ) =>
  StarExecConnection -> Int -> m (Maybe JobPairInfo)
getJobPairInfo (sec, man, cookies) _pairId = do
  let reqStdout = sec { method = "GET"
                      , queryString = "limit=-1"
                      , path = getURL pairStdoutPath [("{pairId}", show _pairId)]
                      , checkStatus = (\_ _ _ -> Nothing)
                      }
      reqLog = sec { method = "GET"
                   , path = getURL pairLogPath [("{pairId}", show _pairId)]
                   , checkStatus = (\_ _ _ -> Nothing)
                   }
  respStdout <- sendRequest (reqStdout, man, cookies)
  if 200 /= (statusCode $ responseStatus respStdout)
    then return Nothing
    else do
      respLog <- sendRequest (reqLog, man, responseCookieJar respStdout)
      return $ Just $ JobPairInfo { jpiPairId = _pairId
                                  , jpiStdout = decodeUtf8Body respStdout
                                  , jpiLog = decodeUtf8Body respLog
                                  }
