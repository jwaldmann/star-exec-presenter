{- |
  StarExecCommands: Module that contains and handles all request to the
  starexec-cluster
-}

module Presenter.StarExec.Commands
  ( getJobResults
  , getJobPairInfo
  , getJobInfo
  , getBenchmarkInfo
  , getSolverInfo
  , getPostProcInfo
  , pushJobXML
  , getSpaceXML
  , getDefaultSpaceXML
  ) where

import Import
import Prelude (head)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO ( stderr )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Presenter.StarExec.Urls
import Presenter.PersistHelper
import Presenter.StarExec.Connection
import qualified Codec.Archive.Zip as Zip
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import Text.HTML.DOM
import Text.HTML.TagSoup
import Text.XML.Cursor
import Codec.Compression.GZip
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Calendar

import Text.Hamlet.XML
import Text.XML
import qualified Data.Char
import Data.CaseInsensitive ()
import Control.Monad ( guard )
import qualified Network.HTTP.Client.MultipartFormData as M
import qualified Network.HTTP.Client as C
import Data.List ( isSuffixOf, mapAccumL )
import Data.Maybe
import Data.Char ( isAlphaNum )

defaultDate :: UTCTime
defaultDate = UTCTime
  (fromGregorian 1970 1 1)
  (secondsToDiffTime 0)

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
                            "unkown"
                            "unkown"
                            False
                            True
                            defaultDate
                            Nothing
                            defaultDate
      getJobStatus t = case t of
                        "complete" -> Complete
                        _ -> Incomplete
      isCompl s = 0 < (T.count "complex" $ T.toLower s)
      parseTDs info xs =
        case xs of
          ("status":t:ts) ->
            parseTDs (info { jobInfoStatus = getJobStatus t }) ts
          ("created":t:ts) ->
            parseTDs (info { jobInfoDate = t }) ts
          ("postprocessor":t:ts) ->
            parseTDs (info { jobInfoPostProc = t }) ts
          ("preprocessor":t:ts) ->
            parseTDs (info { jobInfoPreProc = t }) ts
          ("description":t:ts) ->
            parseTDs (info { jobInfoIsComplexity = isCompl t }) ts
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

constructPostProcInfo :: Int -> Text -> [Cursor] -> PostProcInfo
constructPostProcInfo _procId title tds =
  let basePostProcInfo = PostProcInfo _procId
                                      title
                                      ""
                                      defaultDate
      part (ys,zs) (x:y:xs) = part (x:ys,y:zs) xs
      part rest _           = rest
      (keys,vals) = part ([],[]) tds
      keys' = map (safeHead "" . content) keys
      vals' = map (safeHead "" . content . head . child) vals
      parse info xs = 
        case xs of
          ("description",v):zs -> parse
                                    (info { postProcInfoDescription = v })
                                    zs
          _:zs -> parse info zs
          _ -> info
  in parse basePostProcInfo $ zip keys' vals'

-- | create jobxml DOM according to spec
jobs_to_XML :: [ StarExecJob ] -> Document
jobs_to_XML js = Document (Prologue [] Nothing []) root [] where 
    t x = T.pack $ show x
    b x = T.pack $ map Data.Char.toLower $ show x
    -- path must be in  [_/\w\-\.\+\^=,!?:$%#@ ]*"
    -- but there is a strange '-' in Runtime_Complexity_-_Full_Rewriting etc.
    path_sanitize = T.filter $ \ c -> isAlphaNum c || c `elem` ("/_" :: String)
    root = Element "tns:Jobs" 
             (M.fromList [("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
                         ,("xsi:schemaLocation", "https://www.starexec.org/starexec/public/batchJobSchema.xsd batchJobSchema.xsd")
                         ,("xmlns:tns","https://www.starexec.org/starexec/public/batchJobSchema.xsd") ]) [xml|
       $forall j <- js
          <Job name="#{job_name j}">
            <JobAttributes>
              <description value="#{description j}">
              <queue-id value="#{t $ queue_id j}">
              <start-paused value="#{b $ start_paused j}">
              <cpu-timeout value="#{t $ cpu_timeout j}">
              <wallclock-timeout value="#{t $ wallclock_timeout j}">
              <mem-limit value="#{t $ mem_limit j}">
              <postproc-id value="#{t $ postproc_id j}">
            $forall p <- jobpairs j
                <JobPair job-space-path="#{path_sanitize $ jobPairSpace p}" bench-id="#{t $ jobPairBench p}" config-id="#{t $ jobPairConfig p}">
      |]

-- | need to take care of Issue #34.
-- if jobpairs are like this:
-- [ non-empty, empty, ne, ne, ne, e, ne ]
-- then we produce an archive with the 5 non-empty jobs,
-- and the extra result is
-- [ Just 0, Nothing, Just 1, Just 2, Just 3, Nothing, Just 4 ]
jobs_to_archive :: [ StarExecJob ] -> Maybe (BSL.ByteString, [Maybe Int])
jobs_to_archive js = 
    let empty = null . jobpairs
        ne_js = filter ( not . empty ) js
        ( _, remap ) = mapAccumL 
            ( \ acc j -> if empty j 
            then (acc, Nothing) else (acc + 1, Just acc) ) 0 js
        d = jobs_to_XML ne_js
        e = Zip.toEntry "autojob.xml" 0 ( renderLBS def d ) 
        a = Zip.addEntryToArchive e Zip.emptyArchive
    in  if null ne_js then Nothing 
        else Just ( Zip.fromArchive a, remap )

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


getDefaultSpaceXML :: FilePath -> Handler (Maybe Space)
getDefaultSpaceXML fp = do
    s <- lift $ BSL.readFile fp
    makeSpace s

getSpaceXML :: StarExecConnection -> Int -> Handler (Maybe Space)
getSpaceXML (sec, man, cookies) _spaceId = do
  let req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _spaceId)
                            +> "&type=spaceXML"
                            +> "&includeattrs=false"
                }
  --liftIO $ putStrLn "### getSpaceXML -> ###"
  --liftIO $ print req
  resp <- sendRequest (req, man, cookies)
  --liftIO $ putStrLn "### <- getSpaceXML ###"

  makeSpace $ responseBody resp

makeSpace :: BSL.ByteString -> Handler (Maybe Space)
makeSpace bs = do
  let archive = Zip.toArchive bs
      xml_entries = filter ( \ e -> isSuffixOf ".xml" $ Zip.eRelativePath e ) 
                 $ Zip.zEntries archive 
  let spaces =  case xml_entries of
        [ e ] -> do
          let cursor = cursorFromDOM $ Zip.fromEntry e
              root = laxElement "tns:Spaces" cursor >>= child
              walk :: [ Cursor ] -> [ Space ]
              walk r = r >>= laxElement "Space" >>= \ s -> return
                     Space { spId = case attribute "id" s of
                                 [ i ] -> read $ T.unpack i ; _ -> -1
                           , spName = case attribute "name" s of
                                 [ n ] -> n ; _ -> "noname"
                           , benchmarks = map ( read . T.unpack )
                             $ child s >>= laxElement "benchmark" >>= attribute "id" 
                           , children = child s >>= \ c ->  walk [c]
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
        _solverId solverTitle tds

getPostProcInfo :: StarExecConnection -> Int -> Handler (Maybe PostProcInfo)
getPostProcInfo (sec, man, cookies) _procId = do
  let req = sec { method = "GET"
                , path = postProcPath
                , queryString = "type=post&id=" +> (BSC.pack $ show _procId)
                }
  resp <- sendRequest (req, man, cookies)
  let cursor = cursorFromDOM $ responseBody resp
      procTitle :: Text
      procTitle = getFirstTitle cursor
      procName :: Text
      procName = T.unwords $ filter (  /= "edit" ) $ T.words procTitle
  liftIO $ T.hPutStrLn stderr $ procTitle
  if "http" == T.take 4 procTitle
    then return Nothing
    else do
      let detailFieldset = getFirstFieldset cursor
          tds = getTds detailFieldset
      return $ Just $ constructPostProcInfo
        _procId procName tds

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
              -- liftIO $ BSL.writeFile ((show _jobId) ++ ".csv") $ Zip.fromEntry entry
              let eitherVector = CSV.decodeByName $ Zip.fromEntry entry
              case eitherVector of
                Left msg -> do
                  liftIO $ putStrLn msg
                  return []
                Right (_, jobInfos) ->
                  return $ map insertId $ Vector.toList jobInfos
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
      mPersistJobResult <- getPersistJobResult $ StarExecPairID _pairId
      let resultStatus = case mPersistJobResult of
                            Nothing -> JobResultUndetermined
                            Just jr -> getResultStatus jr
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
      let text = TE.decodeUtf8 $ BSL.toStrict bsl
          tLines = T.lines text
          tContent = drop 1 $ takeWhile isNoSuffixOf tLines
          t = T.dropWhile (/='<') $ T.unlines $ map removeTimeStamp tContent
      in case parseTags t of
            [] -> Nothing
            _ -> Just $ BSL.toStrict $ compress $ BSL.fromStrict $ TE.encodeUtf8 t
    removeTimeStamp t = T.drop 1 $ T.dropWhile (/='\t') t
    isNoSuffixOf line = not $ "EOF" `T.isSuffixOf` line

-- | description of the request object: see
-- org.starexec.command.Connection:uploadXML

pushJobXML :: StarExecConnection -> Int -> [StarExecJob] -> Handler [StarExecJob]
pushJobXML con sId jobs = do
  js <- pushJobXMLStarExec con sId jobs
  registerJobs $ catMaybes $ map jobid js
  return js


pushJobXMLStarExec :: StarExecConnection -> Int -> [StarExecJob] -> Handler [StarExecJob]
pushJobXMLStarExec (sec, man, cookies) sId jobs = case jobs_to_archive jobs of
  Nothing -> return jobs
  Just (bs, remap) -> do
    req <- M.formDataBody [ M.partBS "space" ( BSC.pack $ show sId ) 
         , M.partFileRequestBody "f" "command.zip" $ C.RequestBodyLBS bs
         ] $ sec { path = pushjobxmlPath, responseTimeout = Nothing }
    -- liftIO $ print req
    
    resp <- sendRequest (req, man, cookies)
    -- the job ids are in the returned cookie.
    -- if there are more, then it's a comma-separated list
    -- Cookie {cookie_name = "New_ID", cookie_value = "2818", ... }
    
    let cs = destroyCookieJar $ responseCookieJar resp
    -- liftIO $ print cs
    let vs = do c <- cs ; guard $ cookie_name c == "New_ID" ; return $ cookie_value c
        cut' c s = if null s then []
                  else let (pre,post) = span (/= c) s
                       in  pre : cut' c ( drop 1 post)
    return $ case vs of
         [] ->  jobs
         [s] -> do
             let ids = map read $ cut' ',' 

-- FIXME, actually EXPLAINME:
-- single job: 
-- Cookie {cookie_name = "New_ID", cookie_value = "3048"
-- multiple jobs: 
-- Cookie {cookie_name = "New_ID", cookie_value = "\"3049,3050,3051,3052,3053\""
                     $ filter ( /= '"' ) 

                     $ BSC.unpack s
             (j, mpos) <- zip jobs remap
             let ji = case mpos of 
                     Nothing -> Nothing
                     Just pos -> let i = ids !! pos in
                                 if i > 0 then Just i else Nothing
             return $ j { jobid = ji }
