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
import Database.Persist.Class

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
              queryAction _id
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

runQueryJobInfo :: Int -> Handler (QueryResult QueryInfo (Maybe JobInfo))
runQueryJobInfo = runQueryInfo GetJobInfo UniqueJobInfo queryStarExec
  where queryStarExec _jobId = do
          con <- getConnection
          mJobInfo <- getJobInfo con _jobId
          case mJobInfo of
            Nothing -> return ()
            Just ji -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueJobInfo _jobId
                insertUnique $ ji { jobInfoLastUpdate = currentTime }
              return ()
--runQueryJobInfo _jobId = do
--  let q = GetJobInfo _jobId
--  mPersistJobInfo <- getPersistJobInfo _jobId
--  runQueryBase q $ \mQuery ->
--    case mQuery of
--      Just eq -> return $ pendingQuery (entityKey eq) mPersistJobInfo
--      Nothing -> do
--        mKey <- insertQuery q
--        case mKey of
--          Just queryKey -> do
--            runConcurrent (queryExceptionHandler q) $ do
--              con <- getConnection
--              mJobInfo <- getJobInfo con _jobId
--              case mJobInfo of
--                Nothing -> deleteQuery q
--                Just ji -> do
--                  currentTime <- getTime
--                  _ <- runDB $ do
--                    deleteBy $ UniqueJobInfo _jobId
--                    insertUnique $ ji { jobInfoLastUpdate = currentTime }
--                  deleteQuery q
--                  liftIO $ putStrLn $ "Job done: " ++ (show q)
--            return $ pendingQuery queryKey mPersistJobInfo
--          Nothing -> do
--            mQuery' <- getQuery q
--            case mQuery' of
--              Just eq -> return $ pendingQuery (entityKey eq) mPersistJobInfo
--              -- assuming that the concurrent query is completed
--              Nothing -> do
--                mPersistJobInfo' <- getPersistJobInfo _jobId
--                return $ QueryResult Latest mPersistJobInfo'

runQueryJobResults :: Int -> Handler (QueryResult QueryInfo [JobResultInfo])
runQueryJobResults _jobId = do
  let q = GetJobResults _jobId
  mPersistJobResults <- getPersistJobResults _jobId
  runQueryBase q $ \mQuery ->
    case mQuery of
      Just eq -> do
        return $ pendingQuery (entityKey eq) mPersistJobResults
      Nothing -> do
        mKey <- insertQuery q
        case mKey of
          Just queryKey -> do
            runConcurrent (queryExceptionHandler q) $ do
              con <- getConnection
              results <- getJobResults con _jobId
              if null results
                then deleteQuery q
                else do
                  runDB $ do
                    mapM_ (\r -> deleteBy $ UniqueJobResultInfo $ jobResultInfoPairId r) results
                    mapM_ insertUnique results
                  deleteQuery q
                  liftIO $ putStrLn $ "Job done: " ++ (show q)
            return $ pendingQuery queryKey mPersistJobResults
          Nothing -> do
            mQuery' <- getQuery q
            case mQuery' of
              Just eq -> return $ pendingQuery (entityKey eq) mPersistJobResults
              -- assuming that the concurrent query is completed
              Nothing -> do
                mPersistJobResults' <- getPersistJobResults _jobId
                return $ QueryResult Latest mPersistJobResults'

runQueryJobPair :: Int -> Handler (QueryResult QueryInfo (Maybe JobPairInfo))
runQueryJobPair = runQueryInfo GetJobPair UniqueJobPairInfo queryStarExec
  where queryStarExec _pairId = do
          con <- getConnection
          mJobPair <- getJobPairInfo con _pairId
          case mJobPair of
            Nothing -> return ()
            Just jp -> do
              currentTime <- getTime
              _ <- runDB $ do
                deleteBy $ UniqueJobPairInfo _pairId
                insertUnique jp
              return ()
--runQueryJobPair _pairId = do
--  let q = GetJobPair _pairId
--  mPersistJobPair <- getPersistJobPair _pairId
--  runQueryBase q $ \mQuery ->
--    case mQuery of
--      Just eq -> do
--        return $ pendingQuery (entityKey eq) mPersistJobPair
--      Nothing -> do
--        mKey <- insertQuery q
--        case mKey of
--          Just queryKey -> do
--            runConcurrent (queryExceptionHandler q) $ do
--              con <- getConnection
--              mJobPair <- getJobPairInfo con _pairId
--              case mJobPair of
--                Nothing -> deleteQuery q
--                Just jp -> do
--                  currentTime <- getTime
--                  _ <- runDB $ do
--                    deleteBy $ UniqueJobPairInfo _pairId
--                    insertUnique jp
--                  deleteQuery q
--                  liftIO $ putStrLn $ "Job done: " ++ (show q)
--            return $ pendingQuery queryKey Nothing
--          Nothing -> do
--            mQuery' <- getQuery q
--            case mQuery' of
--              Just eq -> return $ pendingQuery (entityKey eq) mPersistJobPair
--              -- assuming that the concurrent query is completed
--              Nothing -> do
--                mPersistJobPair' <- getPersistJobPair _pairId
--                return $ QueryResult Latest mPersistJobPair'

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
--runQueryBenchmarkInfo _benchId = do
--  let q = GetBenchmarkInfo _benchId
--  mPersistBenchmarkInfo <- getPersistBenchmarkInfo _benchId
--  runQueryBase q $ \mQuery ->
--    case mQuery of
--      Just eq -> do
--        return $ pendingQuery (entityKey eq) mPersistBenchmarkInfo
--      Nothing -> do
--        mKey <- insertQuery q
--        case mKey of
--          Just queryKey -> do
--            runConcurrent (queryExceptionHandler q) $ do
--              con <- getConnection
--              mBenchmarkInfo <- getBenchmarkInfo con _benchId
--              case mBenchmarkInfo of
--                Nothing -> deleteQuery q
--                Just bi -> do
--                  currentTime <- getTime
--                  _ <- runDB $ do
--                    deleteBy $ UniqueBenchmarkInfo _benchId
--                    insertUnique $ bi { benchmarkInfoLastUpdate = currentTime }
--                  deleteQuery q
--                  liftIO $ putStrLn $ "Job done: " ++ (show q)
--            return $ pendingQuery queryKey Nothing
--          Nothing -> do
--            mQuery' <- getQuery q
--            case mQuery' of
--              Just eq -> return $ pendingQuery (entityKey eq) mPersistBenchmarkInfo
--              -- assuming that the concurrent query is completed
--              Nothing -> do
--                mPersistBenchmarkInfo' <- getPersistBenchmarkInfo _benchId
--                return $ QueryResult Latest mPersistBenchmarkInfo'

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
--runQuerySolverInfo _solverId = do
--  let q = GetSolverInfo _solverId
--  mPersistSolverInfo <- getPersistSolverInfo _solverId
--  runQueryBase q $ \mQuery ->
--    case mQuery of
--      Just eq -> do
--        return $ pendingQuery (entityKey eq) mPersistSolverInfo
--      Nothing -> do
--        mKey <- insertQuery q
--        case mKey of
--          Just queryKey -> do
--            runConcurrent (queryExceptionHandler q) $ do
--              con <- getConnection
--              mSolverInfo <- getSolverInfo con _solverId
--              case mSolverInfo of
--                Nothing -> deleteQuery q
--                Just si -> do
--                  currentTime <- getTime
--                  _ <- runDB $ do
--                    deleteBy $ UniqueSolverInfo _solverId
--                    insertUnique $ si { solverInfoLastUpdate = currentTime }
--                  deleteQuery q
--                  liftIO $ putStrLn $ "Job done: " ++ (show q)
--            return $ pendingQuery queryKey Nothing
--          Nothing -> do
--            mQuery' <- getQuery q
--            case mQuery' of
--              Just eq -> return $ pendingQuery (entityKey eq) mPersistSolverInfo
--              -- assuming that the concurrent query is completed
--              Nothing -> do
--                mPersistSolverInfo' <- getPersistSolverInfo _solverId
--                return $ QueryResult Latest mPersistSolverInfo'
