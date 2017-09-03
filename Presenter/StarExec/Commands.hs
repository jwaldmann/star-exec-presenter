{- |
  StarExecCommands: Module that contains and handles all request to the
  starexec-cluster
-}

{-# language LambdaCase #-}

module Presenter.StarExec.Commands
  ( getJobResults
  , getJobPairInfo
  , getJobInfo
  , getBenchmarkInfo
  , getBenchmark
  , getSolverInfo
  , getPostProcInfo
  , pushJobXML
  , getSpaceXML
  , getDefaultSpaceXML
  , pauseJobs , resumeJobs, rerunJobs
  , addJob, addSolver
  , addSpace
  ) where

import Import hiding (spaceId)
import Prelude (head)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO ( stderr )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Presenter.StarExec.Urls
import Presenter.PersistHelper
import Presenter.StarExec.Connection
import Presenter.StarExec.Space
import Presenter.DOI
import qualified Codec.Archive.Zip as Zip
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector
import Text.HTML.TagSoup
import Text.XML.Cursor
import Codec.Compression.GZip
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Calendar
import Text.Printf

import Text.Hamlet.XML
import Text.XML
import qualified Data.Char
import Data.CaseInsensitive ()
import Data.Char (toLower)
import Control.Monad ( guard, when, forM, forM_)
import qualified Network.HTTP.Client.MultipartFormData as M
import qualified Network.HTTP.Client as C
import Data.List (mapAccumL, nub )
import Data.Maybe
import Data.Char ( isAlphaNum, toUpper )
import Control.Monad.Logger

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

-- decodeUtf8Body :: Response BSL.ByteString -> Text
-- decodeUtf8Body = TE.decodeUtf8 . BSL.toStrict . responseBody

-- Source of the exception when request benchmark info
-- There was an exception during a concurrent action:
-- Prelude.head: empty list
-- FIXME: replacing head with safeHead "" is no real solution
getFirstTitle :: Cursor -> Text
getFirstTitle c = safeHead "" $ content h1
  where h1 = safeHead c $ descendant c >>= element "h1" >>= child

getJobInfoFieldset :: Cursor -> Cursor
getJobInfoFieldset c = getFieldsetByID c "detailField"
--getJobInfoFieldset c = head $ descendant c >>= element "fieldset" >>= attributeIs "id" "detailField"

-- Source of the exception when request benchmark info
-- There was an exception during a concurrent action:
-- Prelude.head: empty list
-- FIXME: replacing head with safeHead "" is no real solution
getFirstFieldset :: Cursor -> Cursor
getFirstFieldset c = safeHead c $ getFieldsets c

getFieldsets :: Cursor -> [Cursor]
getFieldsets c = descendant c >>= element "fieldset"

-- Source of the exception when request benchmark info
-- There was an exception during a concurrent action:
-- Prelude.head: empty list
-- FIXME: replacing head with safeHead "" is no real solution
getFieldsetByID :: Cursor -> Text -> Cursor
getFieldsetByID c _id = safeHead c $ getFieldsets c >>= attributeIs "id" _id

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
               <bench-framework value="#{t $ bench_framework j}">
             $forall p <- jobpairs j
               $if isSEJobPair p
                 <JobPair job-space-path="#{path_sanitize $ jobPairSpace p}" bench-id="#{t $ jobPairBench p}" config-id="#{t $ jobPairConfig p}">
      |]

isSEJobPair :: StarExecJobPair -> Bool
isSEJobPair p = case p of
  SEJobPair{} -> True
  _ -> False

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
getJobInfo _ _jobId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , path = jobInfoPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                }
  resp <- sendRequest req
  let cursor = cursorFromDOM $ remove_invalid_html $ responseBody resp
      jobTitle = getFirstTitle cursor
  if "http" == T.take 4 jobTitle
    then return Nothing
    else do
      let fieldset = getJobInfoFieldset cursor
          tds = getTds fieldset
      return $ Just $ constructJobInfo _jobId jobTitle tds



getSpaceXML :: Int -> Handler (Maybe Space)
getSpaceXML _spaceId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _spaceId)
                            +> "&type=spaceXML"
                            +> "&includeattrs=false"
                }
  --liftIO $ putStrLn "### getSpaceXML -> ###"
  --liftIO $ print req
  resp <- sendRequest req
  --liftIO $ putStrLn "### <- getSpaceXML ###"

  makeSpace $ responseBody resp





getBenchmark :: StarExecConnection -> Int -> Handler (BSL.ByteString)
getBenchmark _ bmId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , queryString = "limit=-1"
                , path = getURL benchmarkPath [("{bmId}", show bmId)]
                }
  resp <- sendRequest req
  return $ responseBody resp

getBenchmarkInfo :: StarExecConnection -> Int -> Handler (Maybe BenchmarkInfo)
getBenchmarkInfo _ _benchmarkId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , path = benchmarkInfoPath
                , queryString = "id=" +> (BSC.pack $ show _benchmarkId)
                }
  resp <- sendRequest req
  let cursor = cursorFromDOM $ remove_invalid_html $ responseBody resp
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

remove_invalid_html :: BSLC.ByteString -> BSLC.ByteString
remove_invalid_html =
  let broken  = [ "<! This viewport meta tag should not be deleted. Allows website to render on phones. >"
                , "<!TODO META tags for debugging, delete when finished>"
                ]
      invalid l = BSLC.words l `elem` map BSLC.words broken
  in  BSLC.unlines . filter ( not . invalid ) . BSLC.lines 

getSolverInfo :: StarExecConnection -> Int -> Handler (Maybe SolverInfo)
getSolverInfo _ _solverId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , path = solverInfoPath
                , queryString = "id=" +> (BSC.pack $ show _solverId)
                }
  resp <- sendRequest req
  let cursor = cursorFromDOM $ remove_invalid_html $ responseBody resp
      solverTitle = getFirstTitle cursor
  if "http" == T.take 4 solverTitle
    then return Nothing
    else do
      let detailFieldset = getFirstFieldset cursor
          tds = getTds detailFieldset
      return $ Just $ constructSolverInfo
        _solverId solverTitle tds

getPostProcInfo :: StarExecConnection -> Int -> Handler (Maybe PostProcInfo)
getPostProcInfo _ _procId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , path = postProcPath
                , queryString = "type=post&id=" +> (BSC.pack $ show _procId)
                }
  resp <- sendRequest req
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
getJobResults _ _jobId = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show _jobId)
                                +> "&type=job&returnids=true"
                }
  resp <- sendRequest req

  serv <- doiService <$> getYesod

  let archive = Zip.toArchive $ responseBody resp
      insertId ji = ji { jobResultInfoJobId = _jobId }
      insertDOI ji =
        ji { jobResultInfoBenchmarkDOI
             = toDOI serv $ StarExecBenchmarkID $ jobResultInfoBenchmarkId ji }
  jobs <- case Zip.zEntries archive of
            entry:_ -> do
              -- liftIO $ BSL.writeFile ((show _jobId) ++ ".csv") $ Zip.fromEntry entry
              let eitherVector = CSV.decodeByName $ Zip.fromEntry entry
              case eitherVector of
                Left msg -> do
                  liftIO $ putStrLn msg
                  return []
                Right (_, jobInfos) ->
                  return $ map insertId
                         $ map insertDOI
                         $ Vector.toList jobInfos
            [] -> return []
  return jobs

getJobPairInfo :: StarExecConnection -> Int -> Handler (Maybe JobPairInfo)
getJobPairInfo _ _pairId = do
  sec <- parseUrl starExecUrl
  let reqStdout = sec { method = "GET"
                      , queryString = "limit=-1"
                      , path = getURL pairStdoutPath [("{pairId}", show _pairId)]
                      }
      reqLog = sec { method = "GET"
                   , path = getURL pairLogPath [("{pairId}", show _pairId)]
                   }
  respStdout <- sendRequest reqStdout
  if 200 /= (statusCode $ responseStatus respStdout)
    then return Nothing
    else do
      respLog <- sendRequest reqLog
      mPersistJobResult <- getPersistJobResult $ StarExecPairID _pairId
      let resultStatus = case mPersistJobResult of
                            Nothing -> JobResultUndetermined
                            Just jr -> getResultStatus jr
          stdout = responseBody respStdout
          htmlProof = do
            guard $ not $ BSL.null stdout
            if looks_timestamped stdout
              then getHtmlProof_runsolver stdout
              else getHtmlProof_benchexec stdout
      return $ Just $ JobPairInfo _pairId
                                  (BSL.toStrict $ compress stdout)
                                  (BSL.toStrict $ compress $ responseBody respLog)
                                  htmlProof
                                  resultStatus
  where
    looks_timestamped bsl = and $ do
      l <- take 10 $ T.lines $ TE.decodeUtf8 $ BSL.toStrict bsl
      w <- take 1 $ T.words l
      return $ and $ do c <- T.unpack w ; return $ elem c ("0123456789./" :: String)
    -- drastically simple: if "\n<" occurs among the first 1000 characters,
    -- this this is the start of the "html proof"
    getHtmlProof_benchexec :: BSL.ByteString -> Maybe BS.ByteString
    getHtmlProof_benchexec bsl = do
      let bs = BSL.toStrict bsl
          (early, later) = BS.splitAt 1000 bs
          (pre, post) = BS.breakSubstring "\n<" early
      guard $ BS.length post >= 2
      return $ BSL.toStrict $ compress $ BSL.fromStrict $ BS.drop 1 post <> later
    getHtmlProof_runsolver :: BSL.ByteString -> Maybe BS.ByteString
    getHtmlProof_runsolver bsl =
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

-- | FIXME the method name is wrong because we use pushJobXML *or* createJob
pushJobXML :: JobCreationMethod -> Int -> [StarExecJob] -> Handler [StarExecJob]
pushJobXML meth sId jobs = case meth of
  PushJobXML -> concat <$> (forM jobs $ \ job -> pushJobXML_bulk sId [job] )
  CreateJob  -> concat <$> (forM jobs $ \ job -> createSpaceJob sId job )
    -- createJob sId jobs

createSpaceJob sId job = do
  logWarnN $ T.pack $ "createSpaceJob : " ++ show job
  job' <- makeJobSpace sId job
  createJob sId [ job' ]

-- | make fresh space, copy benchmarks (with sampling), return fresh space id
makeJobSpace :: SpaceID -> StarExecJob -> Handler StarExecJob
makeJobSpace sId job = do
  let name = T.unpack $ "S-" <> description job 
  jobspace <- addSpace sId name name False False False
  forM_ (jobpairs job) $ \ case
    jg @ SEJobGroup {} -> do
      logWarnN $ T.pack $ "copy " ++ show jg
      copySpaces jobspace [jobGroupBench jg] False True (jobGroupSampleRate jg)
      return ()
    other -> do
      logWarnN $ T.pack $ "ignore" ++ show other
  -- FIXME: data StarExecJob / StarExecJobPair is wrong
  -- because it does not model correctly that configs must be identical
  let allconfigs = nub $ do
        jg @ SEJobGroup {} <- jobpairs job
        jobGroupConfigs jg
  return $ job { jobpairs = [ SEJobGroup { jobGroupBench = jobspace
                                         , jobGroupConfigs = allconfigs
                                         , jobGroupSampleRate = 1
                                         }
                            ]
               }

-- FIXME: for large jobs, this is risky (it will no return, but time-out)
-- so we should send jobs one-by-one

pushJobXML_bulk :: Int -> [StarExecJob] -> Handler [StarExecJob]
pushJobXML_bulk sId jobs = do
  mjs <- pushJobXMLStarExec sId jobs
  let js = maybe [] id mjs
  registerJobs $ concat $ catMaybes $ map jobids js
  return js

pushJobXMLStarExec :: Int -> [StarExecJob] -> Handler (Maybe [StarExecJob])
pushJobXMLStarExec sId jobs = case jobs_to_archive jobs of
  Nothing -> return $ Just jobs
  Just (bs, remap) -> do
    sec <- parseUrl starExecUrl
    req <- M.formDataBody [ M.partBS "space" ( BSC.pack $ show sId )
         , M.partFileRequestBody "f" "command.zip" $ C.RequestBodyLBS bs
         ] $ sec { method = "POST"
                 , path = pushjobxmlPath
                 , responseTimeout = responseTimeoutNone
                 }
    let info = mconcat $ do
          job <- jobs
          return $ "Job " <> description job
              <> " num. pairs: " <> T.pack (show $ length $ jobpairs job) <> ","
    logWarnN $ "sending JobXML for " <> info
    -- replace False with True to write the job file to disk
    when (False) $ do
        liftIO $ BSL.writeFile "command.zip" bs

    -- liftIO $ print req

    mresp <- sendRequestMaybe req
    case mresp of
      Nothing -> return Nothing
      Just resp -> do
        let ids = findJobNumber resp

        logWarnN $ "done sending JobXML for " <> info <> " received vs = " <> T.pack (show ids)

        return $ Just $ do
          (j, mpos) <- zip jobs remap
          let ji = case mpos of
                Nothing -> Nothing
                Just pos ->
                  let i = ids !! pos in
                  if i > 0 then Just [i] else Nothing
          return $ j { jobids = ji }

findJobNumber :: Read t => Response body -> [t]
findJobNumber resp =
    -- the job ids are in the returned cookie.
    -- if there are more, then it's a comma-separated list
    -- Cookie {cookie_name = "New_ID", cookie_value = "2818", ... }

    let cs = destroyCookieJar $ responseCookieJar resp

        vs = do c <- cs ; guard $ cookie_name c == "New_ID" ; return $ cookie_value c
        cut' c s = if null s then []
                  else let (pre,post) = span (/= c) s
                       in  pre : cut' c ( drop 1 post)
    in  case vs of
      [] -> []
      [s] -> map read $ cut' ','
-- FIXME, actually EXPLAINME:
-- single job:
-- Cookie {cookie_name = "New_ID", cookie_value = "3048"
-- multiple jobs:
-- Cookie {cookie_name = "New_ID", cookie_value = "\"3049,3050,3051,3052,3053\""
                     $ filter ( /= '"' )
                     $ BSC.unpack s

pauseJobs :: [JobID] -> Handler ()
pauseJobs ids = do
  logWarnN $ "pausing jobs " <> T.pack (show ids)
  _ <- forM ids $ pauseJob
  logWarnN $ "done pausing jobs " <> T.pack (show ids)

pauseJob :: JobID -> Handler ()
pauseJob (StarExecJobID jid) = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "POST"
                , path = getURL pausePath [("{id}", show jid)]
                }
  resp <- sendRequest req
  logWarnN $ T.pack $ show resp
  return ()

resumeJobs :: [JobID] -> Handler ()
resumeJobs ids = do
  logWarnN $ "resuming jobs " <> T.pack (show ids)
  _ <- forM ids $ resumeJob
  logWarnN $ "done resuming jobs " <> T.pack (show ids)

resumeJob :: JobID -> Handler ()
resumeJob (StarExecJobID jid) = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "POST"
                , path = getURL resumePath [("{id}", show jid)]
                }
  resp <- sendRequest req
  logWarnN $ T.pack $ show resp
  return ()

rerunJobs :: [JobID] -> Handler ()
rerunJobs ids = do
  logWarnN $ "re-running jobs " <> T.pack (show ids)
  _ <- forM ids $ rerunJob
  logWarnN $ "done re-running jobs " <> T.pack (show ids)

rerunJob :: JobID -> Handler ()
rerunJob (StarExecJobID jid) = do
  sec <- parseUrl starExecUrl
  let req = sec { method = "POST"
                , path = getURL rerunPath [("{id}", show jid)]
                }
  resp <- sendRequest req
  logWarnN $ T.pack $ show resp
  return ()

data Permission = PaddBench | PremoveBench 
                | PaddJob | PremoveJob
                | PaddSolver | PremoveSolver
                | PaddSpace  | PremoveSpace
                | PaddUser | PremoveUser
                | IsLeader
                deriving (Eq, Ord, Show)

toText :: Permission -> Text
toText p = case show p of
  'P' : cs -> T.pack $ cs
  

{-

2.1 Add Space
URL: add/space
Method: POST
Parameter Encoding: application/x-www-form-urlencoded
Parameters
parent : Integer – The ID of the parent space of this new space.
name : String – The new space name
desc : String – The new space description
locked : Boolean – Whether the space should be locked
users : Boolean – True to inherit all users from the parent space and false otherwise.sticky : Boolean – Whether the new space should have sticky leaders set
+ all permissions parameters except isLeader ( see the permissions section) : Specifies the default permissions
for new users being added to the space.
Description: Creates a new space using the given attributes.
Returns: An HTTP message with 200 status on success, and an HTTP message with an error status on failure.
Return Cookies
New_ID : Integer – On success, contains the ID of the newly created space.
STATUS_MESSAGE_STRING: String – On failure, contains an error message.

-}

addSpace :: SpaceID
  -> String -> String
  -> Bool -> Bool -> Bool
  -> Handler SpaceID
addSpace parent name desc locked users sticky = do
  base <- parseUrl starExecUrl
  let req = urlEncodedBody
            [ ("parent", BSC.pack $ show parent)
            , ("name", BSC.pack name)
            , ("desc", BSC.pack desc)
            , ("locked", boolean locked)
            , ("users", boolean users)
            , ("sticky", boolean sticky)
            ]
            $ base
            { method = "POST"
            , path = getURL addSpacePath []
            }

  logWarnN $ T.pack $ show req
  case requestBody req of
    RequestBodyLBS bs -> logWarnN $ T.pack $ show bs
    _ -> return ()

  resp <- sendRequest req
  when False $ logWarnN $ T.pack $ show resp
  let cs = destroyCookieJar $ responseCookieJar resp
      vs = do c <- cs ; guard $ cookie_name c == "New_ID" ; return $ cookie_value c
  return $ case vs of
    [ s ] -> read $ BSC.unpack s

{-

2.5 Copy Spaces
URL: services/spaces/{spaceId}/copySpace
URL Variables
{spaceId} : Integer – The ID of the space that you want to copy other spaces into
Method: POST
Parameter Encoding: application/x-www-form-urlencoded
Parameters
selectedIds : [Integer] – A list of space IDs, where each space will be copied into the destination space
copyHierarchy : Boolean – True to copy entire space hierarchies into the destination space. False to copy only
the spaces in selectedIds without their hierarchies.
Description: Copies spaces into a single destination space. All benchmarks, solvers, and jobs will be linked
into the newly created spaces.
Returns: A jSON string containing a status object.
Return Cookies
New_ID : [Integer] –A comma-separated list of the new space IDs.

-}

copySpaces :: SpaceID
  -> [SpaceID]
  -> Bool
  -> Bool
  -> Double
  -> Handler [SpaceID]
copySpaces spaceId selectedIds copyPrimitives copyHierarchy sampleRate = do
  base <- parseUrl starExecUrl
  let req = urlEncodedBody
            ( encodeArrayInt "selectedIds" selectedIds ++
              [ ("copyPrimitives", boolean copyPrimitives)
              , ("copyHierarchy", boolean copyHierarchy)
              , ("sampleRate", BSC.pack $ Text.Printf.printf "%3.2f" sampleRate )
              ]
            )
            $ base
            { method = "POST"
            , path = getURL "/starexec/services/spaces/{spaceId}/copySpace"
                     [("{spaceId}", show spaceId)]
            }

  logWarnN $ T.pack $ show req
  case requestBody req of
    RequestBodyLBS bs -> logWarnN $ T.pack $ show bs
    _ -> return ()

  resp <- sendRequest req
  return $ findJobNumber resp

          
{-

copy : Boolean – If true, deep copies of all the given solvers are made first, and then the new solvers are
referenced in the given space. If false, solvers are simply referenced in the new space without being copied.

copyToSubspaces : Boolean – If true, solvers will be associated with every space in the hierarchy rooted at the
given space. Otherwise, they will be associated only with the given space.

fromSpace : integer – If not null, then this is the ID of a space containing all the solvers in selectedIds[] that you
have permission to copy solvers out of. If null, so such space is used, and you must be the owner of the solvers to
have permission to use them.

Description: Given a list of solvers, places the benchmarks into the given space. If copy is true, the
benchmarks are first copied. Otherwise, the benchmarks are just linked into the new space.

Returns: A jSON string containing a status object.

see also http://starexec.lefora.com/topic/58/doc-request-exact-syntax-Integer-urlencoded-parameter

-}

type SpaceID = Int

addSolver :: SpaceID -- ^ toSpace
          -> [(Int,Int)] -- ^ starexec-spaceid, solverid
          -> Bool -- ^ copy
          -> Bool -- ^ copyToSubspaces
          -> Handler ()
addSolver toSpace sids@[(fromSpace,solver)] copy copyToSubspaces = do
  base <- parseUrl starExecUrl

  logWarnN $ T.pack $ unwords [ "addSolver", show toSpace, show sids, show copy, show copyToSubspaces, show fromSpace ]

  let req = urlEncodedBody
            ( encodeArrayInt "selectedIds" (map snd sids) ++
              [ ("copy", boolean copy)
              , ("copyToSubspaces", boolean copyToSubspaces)
              , ("fromSpace", BSC.pack $ show fromSpace)
              ]
            )
            $ base
            { method = "POST"
            , path = getURL addSolverPath [("{spaceId}", show toSpace)]
            }

  logWarnN $ T.pack $ show req
  case requestBody req of
    RequestBodyLBS bs -> logWarnN $ T.pack $ show bs
    _ -> return ()
  resp <- sendRequest req
  logWarnN $ T.pack $ show resp

encodeArrayInt :: BSC.ByteString -> [Int ] -> [(BSC.ByteString,BSC.ByteString)]
encodeArrayInt name xs = do
  x <- xs
  return (name <> "[]" , BSC.pack $ show x)

boolean :: Bool -> BSC.ByteString
boolean flag = BSC.pack $ map toLower $ show flag


data AddJob = AddJob
   { name :: String
   , desc :: String
   , preProcess :: Maybe Int
   , seed :: Int
   , postProcess :: Maybe Int
   , queue :: Int
   , spaceId :: Int
   , cpuTimeout :: Int -- ^ seconds
   , wallclockTimeout :: Int -- ^ seconds
   , maxMem :: Double -- ^ gigabytes
   , pause :: Bool
   , runChoice :: RunChoice
   , configs :: [ Int ] -- ^ Only applies if runChoice is “choose”
   , benchChoice :: BenchChoice -- ^ Only applies if runChoice is “choose”.
   , bench :: [Int] -- ^  The list of benchmarks to use in the job. Only applies if benchChoice is "runChosenFromSpace".
   , traversal :: Traversal
   , suppressTimestamp :: Bool
   , benchFramework :: ! Bench_Framework
   }
   deriving Show

{-
Controls how job pairs are created, and can be either “keepHierarchy” or “choose”. In
“keepHierarchy”, a job is run using all benchmarks that are in the space hierarchy rooted at the spot that the job
was created, and every benchmark is executed by every solver configuration of every solver in the same space.
In “quickJob,” a single job pair is created, using the given solver and the given text to use as a new benchmark.
In “choose”, a list of configurations is provided to use in the job.
-}

data RunChoice = KeepHierarchy | Choose
  deriving (Eq, Show)

{- Describes how to select benchmarks for the job.
Must be one of “runAllBenchInSpace”, “runAllBenchInHierarchy”, “runChosenFromSpace”. If
“runAllBenchInSpace”, all benchmarks in the space the job is being uploaded to will be used. If
"runAllBenchInHierarchy", all benchmarks in the entire hierarchy will be used. If "runChosenFromSpace", then
benchmarks must be provided.
-}

data BenchChoice = RunAllBenchInSpace
                 | RunAllBenchInHierarchy
                 | RunChosenFromSpace
  deriving (Eq, Show)

{-
Controls the order in which job pairs are executed. Can be either “depth” or “robin.” With
“depth,” all the job pairs in a single space will be executed before moving onto another space. With “robin,”
each space in the job will have a single pair executed before any space has a second pair executed, and so on.
-}

data Traversal = Depth | Robin
  deriving (Eq, Show)

{-
Description: Creates a new job with the given parameters.
Returns: An HTTP redirect to the spaces page on success, and an HTTP message with an error code and error
message on failure.
Return Cookies
New_ID : Integer – On success, contains the ID of the new job.
-}

addJob :: AddJob -> Handler (Maybe Int)
addJob c = do
  logWarnN $ "addJob " <> T.pack (show c)

  base <- parseUrl starExecUrl
  req <- encodeParams (
        [ ( "name" , name c )
        , ( "desc",  desc c )
        ] ++
        [ ( "preProcess" , show p ) | p <- maybeToList $ preProcess c ] ++
        [ ( "seed" , show $ seed c ) ] ++
        [ ( "postProcess",  show p ) | p <- maybeToList $ postProcess c ] ++
        [ ( "queue", show $ queue c )
        , ( "sid" , show $ spaceId c )
        , ( "cpuTimeout",  show $ cpuTimeout c )
        , ( "wallclockTimeout", show $ wallclockTimeout c)
        , ( "maxMem" , show $ maxMem c )
        , ( "pause" , if pause c then "yes" else "no" )
        , ( "runChoice" , toLowerHead $ show $ runChoice c ) ] ++
        ( if runChoice c == Choose
          then encodeArrayIntS "configs" $ configs c
          else [] ) ++
        [ ( "benchChoice" , toLowerHead $ show $ benchChoice c) | runChoice c == Choose ] ++
        ( if benchChoice c == RunChosenFromSpace
          then encodeArrayIntS "bench" $ bench c
          else [] ) ++
        [ ( "traversal", toLowerHead $ show $ traversal c )
          -- NOTE: source code says they check for "yes" (not "true"!)
        , ( "suppressTimestamp", if suppressTimestamp c then "yes" else "no" )
        ] ++
        [ ( "benchmarkingFramework", map toUpper $ show $ benchFramework c ) ] 
        ) $ base { method = "POST" , path = addJobPath
                 -- , queryString = "sid=" <> BSC.pack (show $ spaceId c)
                 }

  logWarnN $ T.pack $ show req
  case requestBody req of
    RequestBodyLBS bs -> logWarnN $ T.pack $ show bs
    RequestBodyBS bs -> logWarnN $ T.pack $ show bs
    RequestBodyBuilder i b -> do
      logWarnN "RequestBodyBuilder"
      -- logWarnN $ T.pack $ BSC.unpack $ toByteString b
    RequestBodyStream {} -> logWarnN "RequestBodyStream"
    RequestBodyStreamChunked {} -> logWarnN "RequestBodyStreamChunked"

  mresp <- sendRequestMaybe req

  case mresp of
    Nothing -> return Nothing
    Just resp -> do
      when False $ logWarnN $ T.pack $ show resp
      let cs = destroyCookieJar $ responseCookieJar resp
          vs = do c <- cs ; guard $ cookie_name c == "New_ID" ; return $ cookie_value c
      return $ case vs of
          [ s ] -> Just $ read $ BSC.unpack s
          _ -> Nothing

encodeArrayIntS :: BSC.ByteString -> [Int ] -> [(BSC.ByteString,String)]
encodeArrayIntS name xs = do
  x <- xs
  return (name , show x)

encodeParams :: Monad m => [(BSC.ByteString, String)] -> Request -> m Request
encodeParams kvs req = return
  $ urlEncodedBody ( do (k,v) <- kvs; return (k, BSC.pack v) )
  $ req

-- mpEncodeArrayInt name xs = do
--   (k,v) <- encodeArrayInt name xs
--   return $ MP.partBS (T.pack $ BSC.unpack k) v

toLowerHead :: [Char] -> [Char]
toLowerHead "" = ""
toLowerHead (c:cs) = toLower c : cs

-- | each job in this list must have a singleton list of jobpairs,
-- containing one SEJobGroup
createJob :: Int -> [StarExecJob ] -> Handler [StarExecJob]
createJob spId js = forM js $ \ j -> do
    ids <- forM (jobpairs j) $ \ g -> case g of
      SEJobGroup{} -> do
        mc <- addJob $ AddJob
         { name = T.unpack $ job_name j
         , desc = T.unpack $ description j

         -- error in API doc: cannot omit this, need dummy value
         , preProcess = Just (-1) -- Nothing

         , seed = 0
         , postProcess = Just $ postproc_id j
         , queue = queue_id j
         , cpuTimeout = cpu_timeout j
         , wallclockTimeout = wallclock_timeout j
         , maxMem = mem_limit j
         , pause = start_paused j
         , spaceId = jobGroupBench g
         , benchChoice = RunAllBenchInHierarchy
         , runChoice = Choose
         , configs = jobGroupConfigs g
         , bench = []
         , traversal = Robin
         -- error in API doc: following is required parameter
         , suppressTimestamp = False
         , benchFramework = Benchexec
         }
        logWarnN $ "createJob " <> T.pack (show j) <> " result: " <> T.pack (show mc)
        return mc
      _ -> do
        error $ "Presenter.StarExec.Command.createJob.g: " ++ show g
    return $ j { jobids = Just $ catMaybes ids }
