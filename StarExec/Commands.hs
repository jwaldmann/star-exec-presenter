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
import StarExec.Persist
import StarExec.PersistTypes
import StarExec.Prims (defaultDate)
import qualified Codec.Archive.Zip as Zip
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import Text.HTML.DOM
import Text.XML.Cursor
import Codec.Compression.GZip
import qualified Data.Map as M

import Text.Hamlet.XML
import Text.XML
import qualified Data.Char

-- internal Methods

decodeUtf8Body :: Response BSL.ByteString -> Text
decodeUtf8Body = TE.decodeUtf8 . BSL.toStrict . responseBody

cursorFromDOM :: BSL.ByteString -> Cursor
cursorFromDOM = fromDocument . Text.HTML.DOM.parseLBS

getJobTitle :: Cursor -> Text
getJobTitle c = head $ content h1
  where h1 = head $ descendant c >>= element "h1" >>= child

getJobInfoFieldset :: Cursor -> Cursor
getJobInfoFieldset c = head $ descendant c >>= element "fieldset" >>= attributeIs "id" "detailField"

constructJobInfo :: Int -> Text -> [Cursor] -> JobInfo
constructJobInfo _jobId title tds =
  let baseJobInfo = JobInfo _jobId
                            title
                            Incomplete
                            ""
                            defaultDate
      getJobStatus t = case t of
                        "complete" -> Complete
                        _ -> Incomplete
      parseTDs info xs =
        case xs of
          ("status":t:tds) -> parseTDs
                                (info { jobInfoStatus = getJobStatus t })
                                tds
          ("created":t:tds) -> parseTDs
                                  (info { jobInfoDate = t })
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

data Job =
     Job { postproc_id :: Int
         , description :: T.Text
         , job_name :: T.Text
         , queue_id :: Int
         , mem_limit :: Double
         , wallclock_timeout :: Double
         , cpu_timeout :: Double
         , start_paused :: Bool
         , jobpairs :: [ (Int,Int) ] -- ^ benchmark, config
         }
    deriving Show

-- | create jobxml DOM according to spec
-- jobs_to_XML :: [ Job ] -> XML
jobs_to_XML js = Document (Prologue [] Nothing []) root [] where 
    t x = T.pack $ show x
    b x = T.pack $ map Data.Char.toLower $ show x
    root = Element "tns:Jobs" 
             (M.fromList [("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                         ,("xsi:schemaLocation", "https://www.starexec.org/starexec/public/batchJobSchema.xsd batchJobSchema.xsd")
                         ,("xmlns:tns","https://www.starexec.org/starexec/public/batchJobSchema.xsd") ]) [xml|
       $forall j <- js
           <Job cpu-timeout="#{t $ cpu_timeout j}" description="#{description j}" mem-limit="#{t $ mem_limit j}" name="#{job_name j}" postproc-id="#{t $ postproc_id j}" queue-id="#{t $ queue_id j}" start-paused="#{b $ start_paused j}" wallclock-timeout="#{t $ wallclock_timeout j}">
             $forall bc <- jobpairs j
                 <JobPair bench-id="#{t $ fst bc}" config-id="#{t $ snd bc}">
      |]

-- API

getJobInfo :: StarExecConnection -> Int -> Handler (Maybe JobInfo)
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

-- TODO either Maybe or List^^
getJobResults :: StarExecConnection -> Int -> Handler (Maybe [JobResultInfo])
getJobResults (sec, man, cookies) _jobId = do
  let (+>) = BS.append
      req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                                +> "&type=job&returnids=true"
                }
  resp <- sendRequest (req, man, cookies)
  let archive = Zip.toArchive $ responseBody resp
      insertId ji = ji { jobResultInfoJobId = _jobId }
  jobs <- case Zip.zEntries archive of
            entry:_ -> do
              let eitherVector = CSV.decodeByName $ Zip.fromEntry entry
              case eitherVector of
                Left msg -> do
                  liftIO $ putStrLn msg
                  return Nothing
                Right (_, jobInfos) ->
                  return $ Just $ map insertId $ Vector.toList jobInfos
            [] -> return Nothing
  return jobs

getJobPairInfo :: StarExecConnection -> Int -> Handler (Maybe JobPairInfo)
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
      return $ Just $ JobPairInfo _pairId
                                  (BSL.toStrict $ compress $ responseBody respStdout)
                                  (BSL.toStrict $ compress $ responseBody respLog)

pushJobXml :: StarExecConnection -> Int -> [Job] -> Handler (Maybe [Int])
pushJobXml (sec, man, cookies) spaceId jobs = do
  let (+>) = BS.append
      req = sec { method = "POST"
                , path = pushjobxmlPath
                , queryString = "id=" +> (BSC.pack $ show spaceId)
                                +> "&type=job&returnids=true" -- FIXME
                }
  undefined
