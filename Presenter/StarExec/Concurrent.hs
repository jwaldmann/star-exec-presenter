module Presenter.StarExec.Concurrent 
  ( runQuerySolverInfo
  , runQueryBenchmarkInfo
  , runQueryJobPair
  , runQueryJob
  , runQueryPostProcInfo
  ) where

import Import
import Control.Exception.Base
import Presenter.PersistHelper
import Presenter.Processing
import Presenter.StarExec.Connection
import Presenter.StarExec.Commands
import Data.Time.Clock

{-
  FIXME: possible bug, that QueryInfo won't be deleted from DB after an exception or after a query is complete
-}

type ExceptionHandler = SomeException -> Handler ()

debugTrace :: String -> Handler ()
debugTrace = liftIO . putStrLn

getTime :: Handler UTCTime
getTime = liftIO getCurrentTime

baseExceptionHandler :: SomeException -> Handler ()
baseExceptionHandler e = liftIO $ do
  putStrLn "There was an exception during a concurrent action:"
  putStrLn $ show e

runConcurrent :: ExceptionHandler -> Handler () -> Handler ()
runConcurrent errHandler action = forkHandler errHandler action

runBaseConcurrent :: Handler () -> Handler ()
runBaseConcurrent action = runConcurrent baseExceptionHandler action

getQuery :: SEQuery -> Handler (Maybe (Entity QueryInfo))
getQuery q = runDB $ getBy $ UniqueQueryInfo q

insertQuery :: SEQuery -> Handler (Maybe (Key QueryInfo))
insertQuery q = runDB $ insertUnique $ QueryInfo q

deleteQuery :: SEQuery -> Handler ()
deleteQuery q = runDB $ deleteBy $ UniqueQueryInfo q

pendingQuery :: QueryInfoId -> a -> QueryResult QueryInfo a
pendingQuery key result = QueryResult (Pending key) result

type QueryInfoHandler a = Maybe (Entity QueryInfo) -> Handler (QueryResult QueryInfo a)

queryExceptionHandler :: SEQuery -> ExceptionHandler
queryExceptionHandler q = \e -> do
  baseExceptionHandler e
  deleteQuery q

runQueryBase :: SEQuery -> QueryInfoHandler a -> Handler (QueryResult QueryInfo a)
runQueryBase q handler = do
  mQuery <- getQuery q
  handler mQuery

--runQueryInfo :: (PersistEntityBackend val ~ PersistMonadBackend m?, PersistEntity val) =>
--  (Int -> SEQuery) -> (Int -> Unique val) -> (Int -> Handler ()) -> Int -> Handler (QueryResult QueryInfo (Maybe a))
runQueryInfo queryConstructor uniqueInfoConstructor queryAction _id = do
  let q = queryConstructor _id
  mPersistInfo <- getEntity $ uniqueInfoConstructor _id
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> return $ pendingQuery (entityKey eq) mPersistInfo
      Nothing -> do
        mKey <- insertQuery q
        case mKey of
          Just queryKey -> do
            runConcurrent (queryExceptionHandler q) $ do
              _ <- queryAction _id
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
            return $ pendingQuery queryKey mPersistInfo
          Nothing -> do
            mQuery' <- getQuery q
            case mQuery' of
              Just eq -> return $ pendingQuery (entityKey eq) mPersistInfo
              -- assuming that the concurrent query is completed
              Nothing -> do
                mPersistInfo' <- getEntity $ uniqueInfoConstructor _id
                return $ QueryResult Latest mPersistInfo'

runQueryJob :: Int -> Handler (QueryResult QueryInfo (Maybe JobInfo, [JobResultInfo]))
runQueryJob _jobId = do
  let q = GetJob _jobId
  mPersistJobInfo <- getPersistStarExecJobInfo _jobId
  persistJobResults <- getPersistStarExecJobResults _jobId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> 
        return $ pendingQuery (entityKey eq) (mPersistJobInfo, persistJobResults)
      Nothing -> do
        mKey <- insertQuery q
        case mKey of
          Just queryKey -> do
            runConcurrent (queryExceptionHandler q) $ do
              con <- getConnection
              mJobInfo <- getJobInfo con _jobId
              case mJobInfo of
                Just ji -> do
                  -- liftIO $ putStrLn $ "Got Job: " ++ (show ji)
                  results <- getJobResults con _jobId
                  let isComplexJob = jobInfoIsComplexity ji
                  -- liftIO $ putStrLn $ "Job is complex? " ++ (show isComplexJob)
                  -- liftIO $ putStrLn $ show $ map jobResultInfoStatus results
                  let processedResults =
                        if isComplexJob
                          then (unwrapResults . getScoredResults . wrapResults) results
                          else results

                  runDB_exclusive $ do
                    updateJobInfo mPersistJobInfo ji
                    deleteWhere [JobResultInfoJobId ==. _jobId]
                    mapM_ insertUnique processedResults
                  return ()
                Nothing -> return ()
              liftIO $ putStrLn $ "Job done: " ++ (show q)
              deleteQuery q
            return $ pendingQuery queryKey (mPersistJobInfo, persistJobResults)
          Nothing -> do
            mQuery' <- getQuery q
            case mQuery' of
              Just eq -> do
                persistJobResults' <- getPersistStarExecJobResults _jobId
                return $ pendingQuery (entityKey eq) (mPersistJobInfo, persistJobResults')
              Nothing -> do
                return $ QueryResult Latest (mPersistJobInfo, persistJobResults)

resultIsCompleted :: JobResultInfo -> Bool
resultIsCompleted r = JobResultComplete == jobResultInfoStatus r

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

runQueryBenchmarkInfo :: Int -> Handler (QueryResult QueryInfo (Maybe BenchmarkInfo))
runQueryBenchmarkInfo = runQueryInfo GetBenchmarkInfo UniqueBenchmarkInfo queryStarExec
  where queryStarExec _benchId = do
          con <- getConnection
          mBenchmarkInfo <- getBenchmarkInfo con _benchId
          case mBenchmarkInfo of
            Nothing -> return ()
            Just bi -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueBenchmarkInfo _benchId
                insertUnique $ bi { benchmarkInfoLastUpdate = currentTime }
              return ()

runQuerySolverInfo :: Int -> Handler (QueryResult QueryInfo (Maybe SolverInfo))
runQuerySolverInfo = runQueryInfo GetSolverInfo UniqueSolverInfo queryStarExec
  where queryStarExec _solverId = do
          con <- getConnection
          mSolverInfo <- getSolverInfo con _solverId
          case mSolverInfo of
            Nothing -> return ()
            Just si -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueSolverInfo _solverId
                insertUnique $ si { solverInfoLastUpdate = currentTime }
              return ()

runQueryPostProcInfo :: Int -> Handler (QueryResult QueryInfo (Maybe PostProcInfo))
runQueryPostProcInfo = runQueryInfo GetPostProc UniquePostProcInfo queryStarExec
  where queryStarExec _procId = do
          con <- getConnection
          mPostProcInfo <- getPostProcInfo con _procId
          case mPostProcInfo of
            Nothing -> return ()
            Just pp -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniquePostProcInfo _procId
                insertUnique $ pp { postProcInfoLastUpdate = currentTime }
              return ()
