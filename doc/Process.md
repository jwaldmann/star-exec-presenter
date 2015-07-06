How Requests are Handled
========================

Motivation
----------

there are two levels of caching and it's a bit hard to follow
from the source alone.

Basic ideas for accessing jobpairs etc. are
* if we have a thing already in the database, we don't ask starexec
* if we have marked in the database that we are currently waiting for a response,
  then we don't start another starexec query for the same thing

For displaying competition results, it is no only about data, but about computation
(potentially expensive, needs to look at all results of all provers, for one table).
We use in-memory storage `(TVar (M.Map Key (TVar Value)))` for results of computations

For actually asking star-exec, there is another problem:
we have to be logged in, and apparently, sessions do vanish,
so we might need to re-authenticate. This is handled by `getConnection`,
which is (or should be) a point of synchronization (if we get new
credentials, we need to use them from now on).


Example
-------

A request like this comes in:
http://nfa.imn.htwk-leipzig.de/termcomp-devel/pairs/116580546

because of
```
/pairs/#JobPairID ShowJobPairR GET
```
in config/routes

it gets handled im module `Handler.ShowJobPair` by
```
getShowJobPairR :: JobPairID -> Handler Html
getShowJobPairR pid@(StarExecPairID _id) = do
  logWarnN $ T.pack $ "getShowJobPairR.pid = " ++ show pid
  qr @ (QueryResult qStatus mPair) <- queryJobPair pid
  logWarnN $ T.pack $ "getShowJobPairR.qr =" ++ show qr
  mJobResult <- getPersistJobResult pid
  logWarnN $ T.pack $ "getShowJobPairR.mJobResult =" ++ show mJobResult
  (mj, mb, ms) <- case mJobResult of
                    Just (StarExecResult jr) -> do
```
module `Presenter.StarExec.JobData' :

```
queryJobPair :: JobPairID -> Handler (QueryResult QueryInfo (Maybe Pair))
queryJobPair _pairId@(StarExecPairID pid) = do
  logWarnN $ T.pack $ "queryJobPairR._pairId = " ++ show _pairId
  mPersistPairInfo <- getPersistJobPair _pairId
  logWarnN $ T.pack $ "queryJobPairR.mPersistPairInfo = " ++ show mPersistPairInfo
  case mPersistPairInfo of
    Just (StarExecPair persistPairInfo) -> do
      if jobPairInfoResultStatus persistPairInfo == JobResultComplete
        then return $ QueryResult Latest mPersistPairInfo
        else runQueryJobPair pid >>= wrap (fmap StarExecPair)
    _ -> runQueryJobPair pid >>= wrap (fmap StarExecPair)
```
module `Presenter.StarExec.Concurrent`:
```
runQueryJobPair :: Int -> Handler (QueryResult QueryInfo (Maybe JobPairInfo))
runQueryJobPair = runQueryInfo GetJobPair UniqueJobPairInfo queryStarExec
  where queryStarExec _pairId = do
          con <- getConnection
          mJobPair <- getJobPairInfo con _pairId
          case mJobPair of
            Nothing -> return ()
            Just jp -> do
              _ <- runDB $ do
                deleteBy $ UniqueJobPairInfo _pairId
                insertUnique jp
              return ()
```
same module
```
runQueryInfo queryConstructor uniqueInfoConstructor queryAction _id = do
  let q = queryConstructor _id
  logWarnN $ T.pack $ "runQueryInfo._id = " ++ show _id
  mPersistInfo <- getEntity $ uniqueInfoConstructor _id
  logWarnN $ T.pack $ "runQueryInfo.mPersistInfo = " ++ show mPersistInfo
  runQueryBase q $ \mQuery -> do
    logWarnN $ T.pack $ "runQueryInfo.mQuery = " ++ show mQuery
    case mQuery of
      Just eq -> do
        return $ pendingQuery (entityKey eq) mPersistInfo
      Nothing -> do
        mKey <- insertQuery q
        logWarnN $ T.pack $ "runQueryInfo.mKey = " ++ show mKey
        case mKey of
          Just queryKey -> do
            runConcurrent (queryExceptionHandler q) $ do
              _ <- queryAction _id
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
            return $ pendingQuery queryKey mPersistInfo
          Nothing -> do
            mQuery' <- getQuery q
            logWarnN $ T.pack $ "runQueryInfo.mQuery' = " ++ show mQuery'
            case mQuery' of
              Just eq -> return $ pendingQuery (entityKey eq) mPersistInfo
              -- assuming that the concurrent query is completed
              Nothing -> do
                mPersistInfo' <- getEntity $ uniqueInfoConstructor _id
                logWarnN $ T.pack $ "runQueryInfo.mPersistInfo' = " ++ show mPersistInfo'
                return $ QueryResult Latest mPersistInfo'
```



Log Data
--------

When handling request http://localhost:3000/pairs/115506327, this happens
(and it feels way too much)


```
06/Jul/2015:18:12:02 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:12:02 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:12:02 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mQuery = Nothing
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mKey = Just (QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}})
06/Jul/2015:18:12:02 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:12:02 +0200 [Warn] getConnection
06/Jul/2015:18:12:02 +0200 [Warn] getConnection
06/Jul/2015:18:12:02 +0200 [Warn] getConnection.mSession: Nothing
06/Jul/2015:18:12:02 +0200 [Warn] sendRequest: Request {
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mQuery = Nothing
06/Jul/2015:18:12:02 +0200 [Warn] runQueryInfo.mKey = Just (QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}})
06/Jul/2015:18:12:02 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:12:12 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:12:12 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:12:12 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:12:12 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:12:12 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:12:12 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:12:15 +0200 [Warn] done sendRequest: Request {
06/Jul/2015:18:12:15 +0200 [Warn] login ...
06/Jul/2015:18:12:15 +0200 [Warn] checkLogin ...
06/Jul/2015:18:12:15 +0200 [Warn] sendRequest: Request {
06/Jul/2015:18:12:21 +0200 [Warn] done sendRequest: Request {
06/Jul/2015:18:12:21 +0200 [Warn] checkLogin: False
06/Jul/2015:18:12:21 +0200 [Warn] we are not logged in
06/Jul/2015:18:12:21 +0200 [Warn] sendRequest: Request {
06/Jul/2015:18:12:22 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:12:22 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:12:22 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:12:22 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:12:22 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:12:22 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:12:33 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:12:33 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:12:33 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:33 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:12:33 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:12:33 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:12:43 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:12:43 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:12:43 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:43 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:12:43 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:12:43 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:12:52 +0200 [Warn] done sendRequest: Request {
06/Jul/2015:18:12:52 +0200 [Warn] getConnection - before writeExclusive
06/Jul/2015:18:12:52 +0200 [Warn] getConnection - after  writeExclusive
06/Jul/2015:18:12:52 +0200 [Warn] sendRequest: Request {
06/Jul/2015:18:12:52 +0200 [Warn] getConnection.mSession: Just (SessionData {cookieData = CJ {expose = [Cookie {cookie_name = "JSESSIONID", cookie_value = "4657496F494A1EF2AC3184606CC8FBC9", cookie_expiry_time = 3014-11-06 00:00:00 UTC, cookie_domain = "www.starexec.org", cookie_path = "/starexec/", cookie_creation_time = 2015-07-06 16:12:15.335826 UTC, cookie_last_access_time = 2015-07-06 16:12:48.918867 UTC, cookie_persistent = False, cookie_host_only = True, cookie_secure_only = True, cookie_http_only = True}]}, date = 2015-07-06 16:12:02.677907 UTC})
06/Jul/2015:18:12:52 +0200 [Warn] sendRequest: Request {
06/Jul/2015:18:12:52 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:12:52 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:12:52 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:12:52 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:12:52 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:12:52 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:13:03 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:13:03 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:13:03 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:13:03 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:13:03 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:13:03 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:13:09 +0200 [Warn] done sendRequest: Request {
06/Jul/2015:18:13:09 +0200 [Warn] sendRequest: Request {
06/Jul/2015:18:13:13 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:13:13 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:13:13 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:13:13 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:13:13 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:13:13 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3021}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
06/Jul/2015:18:13:24 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:13:24 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3020}}, entityVal = QueryInfo {queryInfoQuery = GetJobPair 115506327}})
06/Jul/2015:18:13:24 +0200 [Warn] getShowJobPairR.qr =QueryResult {queryStatus = Pending _, queryResult = Nothing}
06/Jul/2015:18:13:24 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mQuery = Nothing
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mKey = Just (QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3022}})
06/Jul/2015:18:13:24 +0200 [Warn] getConnection
06/Jul/2015:18:13:24 +0200 [Warn] queryJobPairR.mPersistPairInfo = Nothing
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo._id = 115506327
06/Jul/2015:18:13:24 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:30 +0200 [Warn] done sendRequest: Request {
06/Jul/2015:18:13:34 +0200 [Warn] getShowJobPairR.pid = StarExecPairID 115506327
06/Jul/2015:18:13:34 +0200 [Warn] queryJobPairR._pairId = StarExecPairID 115506327
06/Jul/2015:18:13:34 +0200 [Warn] queryJobPairR.mPersistPairInfo = Just (StarExecPair (JobPairInfo {jobPairInfoPairId = 115506327, jobPairInfoStdout = "\US\139\b\NUL\NUL\NUL ...
06/Jul/2015:18:13:34 +0200 [Warn] getShowJobPairR.mJobResult =Just (StarExecResult (JobResultInfo {jobResultInfoJobId = 8287, jobResultInfoScore = Nothing, jobResultInfoPairId = 115506327, jobResultInfoBenchmark = "TRS_Standard/AG01/#3.48.xml", jobResultInfoBenchmarkId = 1123279, jobResultInfoSolver = "aprove 2015 test 8", jobResultInfoSolverId = 3342, jobResultInfoConfiguration = "certified", jobResultInfoConfigurationId = 22444, jobResultInfoStatus = JobResultComplete, jobResultInfoCpuTime = 2.40163, jobResultInfoWallclockTime = 1.62596, jobResultInfoResult = YES Nothing}))
06/Jul/2015:18:13:34 +0200 [Warn] runQueryInfo._id = 1123279
06/Jul/2015:18:13:34 +0200 [Warn] runQueryInfo.mPersistInfo = Nothing
06/Jul/2015:18:13:34 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3019}}, entityVal = QueryInfo {queryInfoQuery = GetBenchmarkInfo 1123279}})
06/Jul/2015:18:13:34 +0200 [Warn] runQueryInfo._id = 3342
06/Jul/2015:18:13:34 +0200 [Warn] runQueryInfo.mPersistInfo = Just (SolverInfo {solverInfoStarExecId = 3342, solverInfoName = "aprove 2015 test 8", solverInfoDescription = "no description", solverInfoLastUpdate = 2015-07-06 12:20:56.667605 UTC})
06/Jul/2015:18:13:34 +0200 [Warn] runQueryInfo.mQuery = Just (Entity {entityKey = QueryInfoKey {unQueryInfoKey = SqlBackendKey {unSqlBackendKey = 3022}}, entityVal = QueryInfo {queryInfoQuery = GetSolverInfo 3342}})
```

