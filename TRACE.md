How a query is handled:
-----------------------

in a browser, we type  http://localhost:3000/pairs/26916257

config/routes contains
```
/pairs/#JobPairID ShowJobPairR GET
```
so the following handler is called:
```
module Handler.ShowJobPair where
getShowJobPairR :: JobPairID -> Handler Html
getShowJobPairR pid@(StarExecPairID _id) = do
```

There is a call
```
  qr @ (QueryResult qStatus mPair) <- queryJobPair pid
```

this goes to
```
module Presenter.StarExec.JobData
queryJobPair :: JobPairID -> Handler (QueryResult QueryInfo (Maybe Pair))
queryJobPair _pairId@(StarExecPairID pid) = do
```

There is a call
```
  mPersistPairInfo <- getPersistJobPair _pairId
```
this goes to
```
module Presenter.PersistHelper where
getPersistJobPair :: JobPairID -> Handler (Maybe Pair)
getPersistJobPair = runDB . getPersistJobPair'

getPersistJobPair' :: JobPairID -> YesodDB App (Maybe Pair)
getPersistJobPair' (StarExecPairID _id) = getEntityVal' (UniqueJobPairInfo _id) StarExecPair

getEntityVal' uniqueVal dataConstructor = do
  mVal <- getEntity' uniqueVal
  return $ dataConstructor <$> mVal

getEntity' uniqueVal = do
  mVal <- getBy uniqueVal
  return $ entityVal <$> mVal

getEntity uniqueVal = runDB_readlocked $ getEntity' uniqueVal
```
Now we handle the value that was returned from the DB lookup
```
  case mPersistPairInfo of
    Just (StarExecPair persistPairInfo) -> do
      if jobPairInfoResultStatus persistPairInfo == JobResultComplete
        then return $ QueryResult Latest mPersistPairInfo
        else runQueryJobPair pid >>= wrap (fmap StarExecPair)
    _ -> runQueryJobPair pid >>= wrap (fmap StarExecPair)
```
If the status is ..Complete, then we are done. Good.
If the status is  not ..Complete, then we run a query?
We must check that we do not start too many queries!

```
module Presenter.StarExec.Concurrent

runQueryJobPair = runQueryInfo GetJobPair UniqueJobPairInfo queryStarExec
  where queryStarExec _pairId = do
          con <- getConnection
          mJobPair <- getJobPairInfo con _pairId
          case mJobPair of
            Nothing -> return ()
            Just jp -> do
              _ <- runDB_writelocked $ do
                deleteBy $ UniqueJobPairInfo _pairId
                insertUnique jp
              return ()
```
