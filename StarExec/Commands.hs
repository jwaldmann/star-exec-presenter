{- |
  StarExecCommands: Module that contains and handles all request to the
  starexec-cluster
-}

module StarExec.Commands
  ( getJobResults
  , getJobPairInfo
  , getJobInfo
  , getBenchmarkInfo
  , getSolverInfo
  , pushJobXML, Job (..) -- FIXME: move to some other module
  , getSpaceXML
  ) where

import Import
import Prelude (head, Maybe(..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import StarExec.Types
import StarExec.Urls
import StarExec.Persist
import StarExec.Connection
import StarExec.Prims (defaultDate)
import qualified Codec.Archive.Zip as Zip
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import Text.HTML.DOM
import Text.HTML.TagSoup
import Text.XML.Cursor
import Codec.Compression.GZip
import qualified Data.Map as M

import Text.Hamlet.XML
import Text.XML
import qualified Data.Char
import Data.CaseInsensitive ()
import Control.Monad ( guard )
import qualified Network.HTTP.Client.MultipartFormData as M
import qualified Network.HTTP.Client as C
import Data.List ( isSuffixOf )

(+>) :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
(+>) = BS.append

safeHead :: a -> [a] -> a
safeHead _ (x:_) = x
safeHead defaultVal [] = defaultVal

-- * internal Methods

decodeUtf8Body :: Response BSL.ByteString -> Text
decodeUtf8Body = TE.decodeUtf8 . BSL.toStrict . responseBody

cursorFromDOM :: BSL.ByteString -> Cursor
cursorFromDOM = fromDocument . Text.HTML.DOM.parseLBS

getFirstTitle :: Cursor -> Text
getFirstTitle c = head $ content h1
  where h1 = head $ descendant c >>= element "h1" >>= child

getJobInfoFieldset :: Cursor -> Cursor
getJobInfoFieldset c = getFieldsetByID c "detailField"
--getJobInfoFieldset c = head $ descendant c >>= element "fieldset" >>= attributeIs "id" "detailField"

getFirstFieldset :: Cursor -> Cursor
getFirstFieldset c = head $ getFieldsets c

getFieldsets :: Cursor -> [Cursor]
getFieldsets c = descendant c >>= element "fieldset"

getFieldsetByID :: Cursor -> Text -> Cursor
getFieldsetByID c _id = head $ getFieldsets c >>= attributeIs "id" _id

getTds :: Cursor -> [Cursor]
getTds c = descendant c >>= element "td" >>= child

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
          ("status":t:ts) -> parseTDs
                                (info { jobInfoStatus = getJobStatus t })
                                ts
          ("created":t:ts) -> parseTDs
                                  (info { jobInfoDate = t })
                                  ts
          (_:ts) -> parseTDs info ts
          _ -> info
      tds' = map (safeHead "" . content) tds
  in parseTDs baseJobInfo tds'

constructBenchmarkInfo :: Int -> Text -> [Cursor] -> BenchmarkInfo
constructBenchmarkInfo _benchmarkId title tds =
  let baseBenchmarkInfo = BenchmarkInfo _benchmarkId
                                        title
                                        ""
                                        defaultDate
      parseTDs info xs =
        case xs of
          ("name":t:ts) -> parseTDs
                              (info { benchmarkInfoType = t })
                              ts
          (_:ts) -> parseTDs info ts
          _ -> info
      tds' = map (safeHead "" . content) tds
  in parseTDs baseBenchmarkInfo tds'

constructSolverInfo :: Int -> Text -> [Cursor] -> SolverInfo
constructSolverInfo _solverId title tds =
  let baseSolverInfo = SolverInfo _solverId
                                  title
                                  ""
                                  defaultDate
      parseTDs info xs =
        case xs of
          ("description":t:ts) -> parseTDs
                                     (info { solverInfoDescription = t })
                                     ts
          (_:ts) -> parseTDs info ts
          _ -> info
      tds' = map (safeHead "" . content) tds
  in parseTDs baseSolverInfo tds'

data Job =
     Job { postproc_id :: Int
         , description :: T.Text
         , job_name :: T.Text
         , queue_id :: Int
         , mem_limit :: Double
         , wallclock_timeout :: Int
         , cpu_timeout :: Int
         , start_paused :: Bool
         , jobpairs :: [ (Int,Int) ] -- ^ benchmark, config
         , jobid :: Maybe Int
         }
    deriving Show

-- | create jobxml DOM according to spec
jobs_to_XML :: [ Job ] -> Document
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

jobs_to_archive :: [ Job ] -> Maybe BSL.ByteString
jobs_to_archive js = 
    let empty = null . jobpairs
        ne_js = filter ( not . empty ) js
        d = jobs_to_XML ne_js
        e = Zip.toEntry "autojob.xml" 0 ( renderLBS def d ) 
        a = Zip.addEntryToArchive e Zip.emptyArchive
    in  if null ne_js then Nothing else Just $ Zip.fromArchive a

-- * API

getJobInfo :: StarExecConnection -> Int -> Handler (Maybe JobInfo)
getJobInfo (sec, man, cookies) _jobId = do
  let req = sec { method = "GET"
                , path = jobInfoPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                }
  resp <- sendRequest (req, man, cookies)
  let cursor = cursorFromDOM $ responseBody resp
      jobTitle = getFirstTitle cursor
  if "http" == T.take 4 jobTitle
    then return Nothing
    else do
      let fieldset = getJobInfoFieldset cursor
          tds = getTds fieldset
      return $ Just $ constructJobInfo _jobId jobTitle tds

getSpaceXML :: StarExecConnection -> Int -> Handler (Maybe Space)
getSpaceXML (sec, man, cookies) _spaceId = do
  let req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _spaceId)
                            +> "&type=spaceXML"
                            +> "&includeattrs=false"
                }
  resp <- sendRequest (req, man, cookies)

  let archive = Zip.toArchive $ responseBody resp
      xml_entries = filter ( \ e -> isSuffixOf ".xml" $ Zip.eRelativePath e ) 
                 $ Zip.zEntries archive 
  let spaces =  case xml_entries of
        [ e ] -> do
          let c = cursorFromDOM $ Zip.fromEntry e
              root = laxElement "tns:Spaces" c >>= child
              walk :: [ Cursor ] -> [ Space ]
              walk root = root >>= laxElement "Space" >>= \ s -> return
                     Space { benchmarks = map ( read . T.unpack )
                             $ child s >>= laxElement "benchmark" >>= attribute "id" 
                           , children = [] -- FIXME: child s >>= walk 
                           }
          walk root
        _ -> []

  case spaces of
      [s] -> return $ Just s
      _ -> do
          liftIO $ putStrLn "====== no space ======"
          return Nothing
    

getBenchmarkInfo :: StarExecConnection -> Int -> Handler (Maybe BenchmarkInfo)
getBenchmarkInfo (sec, man, cookies) _benchmarkId = do
  let req = sec { method = "GET"
                , path = benchmarkInfoPath
                , queryString = "id=" +> (BSC.pack $ show _benchmarkId)
                }
  resp <- sendRequest (req, man, cookies)
  let cursor = cursorFromDOM $ responseBody resp
      benchmarkTitle = getFirstTitle cursor
  if "http" == T.take 4 benchmarkTitle
    then return Nothing
    else do
      let detailFieldset = getFirstFieldset cursor
          typeFieldset = getFieldsetByID cursor "fieldType"
          detailTds = getTds detailFieldset
          typeTds = getTds typeFieldset
      return $ Just $ constructBenchmarkInfo
        _benchmarkId benchmarkTitle $ detailTds ++ typeTds

getSolverInfo :: StarExecConnection -> Int -> Handler (Maybe SolverInfo)
getSolverInfo (sec, man, cookies) _solverId = do
  let req = sec { method = "GET"
                , path = solverInfoPath
                , queryString = "id=" +> (BSC.pack $ show _solverId)
                }
  resp <- sendRequest (req, man, cookies)
  let cursor = cursorFromDOM $ responseBody resp
      solverTitle = getFirstTitle cursor
  if "http" == T.take 4 solverTitle
    then return Nothing
    else do
      let detailFieldset = getFirstFieldset cursor
          tds = getTds detailFieldset
      return $ Just $ constructSolverInfo
        _solverId solverTitle $ tds

getJobResults :: StarExecConnection -> Int -> Handler [JobResultInfo]
getJobResults (sec, man, cookies) _jobId = do
  let req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                                +> "&type=job&returnids=true"
                }
  resp <- sendRequest (req, man, cookies)
  let archive = Zip.toArchive $ responseBody resp
      insertId ji = ji { jobResultInfoJobId = _jobId }
  jobs <- case Zip.zEntries archive of
            entry:_ -> do
              --liftIO $ BSL.writeFile ((show _jobId) ++ ".csv") $ Zip.fromEntry entry
              let eitherVector = CSV.decodeByName $ BSL.map filterWord8 $ Zip.fromEntry entry
              case eitherVector of
                Left msg -> do
                  liftIO $ putStrLn msg
                  return []
                Right (_, jobInfos) ->
                  return $ map insertId $ Vector.toList jobInfos
              where
                filterWord8 :: Word8 -> Word8
                filterWord8 34 = 39
                filterWord8 w = w
            [] -> return []
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
      mPersistJobResult <- getPersistJobResult _pairId
      let resultStatus = case mPersistJobResult of
                            Nothing -> JobResultUndetermined
                            Just jr -> jobResultInfoStatus jr
          stdout = responseBody respStdout
          htmlProof = getHtmlProof stdout
      return $ Just $ JobPairInfo _pairId
                                  (BSL.toStrict $ compress stdout)
                                  (BSL.toStrict $ compress $ responseBody respLog)
                                  htmlProof
                                  resultStatus
  where
    getHtmlProof :: BSL.ByteString -> Maybe BS.ByteString
    getHtmlProof bsl =
      let t = TE.decodeUtf8 $ BSL.toStrict bsl
          mHtmlStart = T.findIndex (=='<') t
      in case mHtmlStart of
          Nothing -> Nothing
          Just hs -> 
            let htmlText = T.drop hs t
            in case parseTags htmlText of
                [] -> Nothing
                _ -> Just $ BSL.toStrict $ compress $ BSL.fromStrict $ TE.encodeUtf8 htmlText

-- | description of the request object: see
-- org.starexec.command.Connection:uploadXML

pushJobXML :: StarExecConnection -> Int -> [Job] -> Handler [Job]
pushJobXML (sec, man, cookies) spaceId jobs = case jobs_to_archive jobs of
  Nothing -> return jobs
  Just bs -> do
    req <- M.formDataBody [ M.partBS "space" ( BSC.pack $ show spaceId ) 
         , M.partFileRequestBody "f" "command.zip" $ C.RequestBodyLBS bs
         ] $ sec { path = pushjobxmlPath }
    liftIO $ print req 

    -- liftIO $ BSL.writeFile "command.zip" bs
    
    resp <- sendRequest (req, man, cookies)
    -- the job ids are in the returned cookie.
    -- if there are more, then it's a comma-separated list
    -- Cookie {cookie_name = "New_ID", cookie_value = "2818", ... }
    
    let cs = destroyCookieJar $ responseCookieJar resp
    liftIO $ print cs
    let vs = do c <- cs ; guard $ cookie_name c == "New_ID" ; return $ cookie_value c
        cut c s = if null s then []
                  else let (pre,post) = span (/= c) s
                       in  pre : cut c ( drop 1 post)
    return $ case vs of
         [] ->  jobs
         [s] -> do
             let ids = map read $ cut ',' 

-- FIXME, actually EXPLAINME:
-- single job: 
-- Cookie {cookie_name = "New_ID", cookie_value = "3048"
-- multiple jobs: 
-- Cookie {cookie_name = "New_ID", cookie_value = "\"3049,3050,3051,3052,3053\""
                     $ filter ( /= '"' ) 

                     $ BSC.unpack s
             (j,i) <- zip jobs ids
             return $ j { jobid = do guard $ i > 0 ; return i }
