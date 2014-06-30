module StarExec.Concurrent 
  ( runQueryJobInfo
  , runQueryJobResults
  , runQuerySolverInfo
  , runQueryBenchmarkInfo
  , runQueryJobPair
  ) where

import Import
import Control.Exception.Base
import StarExec.Types
import StarExec.PersistTypes
import StarExec.Persist
import StarExec.Connection
import StarExec.Commands
import Data.Maybe
import Data.Time.Clock

type ExceptionHandler = SomeException -> Handler ()

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

runQueryJobInfo :: Int -> Handler (QueryResult QueryInfo (Maybe JobInfo))
runQueryJobInfo _jobId = do
  let q = GetJobInfo _jobId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> do
        mPersistJobInfo <- getPersistJobInfo _jobId
        return $ pendingQuery (entityKey eq) mPersistJobInfo
      Nothing -> do
        mKey <- insertQuery q
        runConcurrent (queryExceptionHandler q) $ do
          con <- getConnection
          mJobInfo <- getJobInfo con _jobId
          case mJobInfo of
            Nothing -> deleteQuery q
            Just ji -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueJobInfo _jobId
                insertUnique $ ji { jobInfoLastUpdate = currentTime }
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
        return $ pendingQuery (fromJust mKey) Nothing

runQueryJobResults :: Int -> Handler (QueryResult QueryInfo [JobResultInfo])
runQueryJobResults _jobId = do
  let q = GetJobResults _jobId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> do
        mPersistJobResults <- getPersistJobResults _jobId
        return $ pendingQuery (entityKey eq) mPersistJobResults
      Nothing -> do
        mKey <- insertQuery q
        runConcurrent (queryExceptionHandler q) $ do
          con <- getConnection
          mResults <- getJobResults con _jobId
          case mResults of
            Nothing -> deleteQuery q
            Just rs -> do
              runDB $ do
                mapM_ (\r -> deleteBy $ UniqueJobResultInfo $ jobResultInfoPairId r) rs
                mapM_ insertUnique rs
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
        return $ pendingQuery (fromJust mKey) []

runQueryJobPair :: Int -> Handler (QueryResult QueryInfo (Maybe JobPairInfo))
runQueryJobPair _pairId = do
  let q = GetJobPair _pairId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> do
        mPersistJobPair <- getPersistJobPair _pairId
        return $ pendingQuery (entityKey eq) mPersistJobPair
      Nothing -> do
        mKey <- insertQuery q
        runConcurrent (queryExceptionHandler q) $ do
          con <- getConnection
          mJobPair <- getJobPairInfo con _pairId
          case mJobPair of
            Nothing -> deleteQuery q
            Just jp -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueJobPairInfo _pairId
                insertUnique jp
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
        return $ pendingQuery (fromJust mKey) Nothing

runQueryBenchmarkInfo :: Int -> Handler (QueryResult QueryInfo (Maybe BenchmarkInfo))
runQueryBenchmarkInfo _benchId = do
  let q = GetBenchmarkInfo _benchId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> do
        mPersistBenchmarkInfo <- getPersistBenchmarkInfo _benchId
        return $ pendingQuery (entityKey eq) mPersistBenchmarkInfo
      Nothing -> do
        mKey <- insertQuery q
        runConcurrent (queryExceptionHandler q) $ do
          con <- getConnection
          mBenchmarkInfo <- getBenchmarkInfo con _benchId
          case mBenchmarkInfo of
            Nothing -> deleteQuery q
            Just bi -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueBenchmarkInfo _benchId
                insertUnique $ bi { benchmarkInfoLastUpdate = currentTime }
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
        return $ pendingQuery (fromJust mKey) Nothing

runQuerySolverInfo :: Int -> Handler (QueryResult QueryInfo (Maybe SolverInfo))
runQuerySolverInfo _solverId = do
  let q = GetSolverInfo _solverId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> do
        mPersistSolverInfo <- getPersistSolverInfo _solverId
        return $ pendingQuery (entityKey eq) mPersistSolverInfo
      Nothing -> do
        mKey <- insertQuery q
        runConcurrent (queryExceptionHandler q) $ do
          con <- getConnection
          mSolverInfo <- getSolverInfo con _solverId
          case mSolverInfo of
            Nothing -> deleteQuery q
            Just si -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueSolverInfo _solverId
                insertUnique $ si { solverInfoLastUpdate = currentTime }
              deleteQuery q
              liftIO $ putStrLn $ "Job done: " ++ (show q)
        return $ pendingQuery (fromJust mKey) Nothing
