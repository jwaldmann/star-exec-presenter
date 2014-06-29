module StarExec.Concurrent 
  ( runQuery
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

pendingQuery :: QueryInfoId -> QueryIntermediateResult -> QueryResult QueryInfo QueryIntermediateResult
pendingQuery key result = QueryResult (Pending key) result

runQuery :: SEQuery -> Handler (QueryResult QueryInfo QueryIntermediateResult)
runQuery q@(GetJobInfo _jobId) = do
  mQuery <- getQuery q -- runDB $ getBy $ UniqueQueryInfo q
  case mQuery of
    Just query -> do
      mPersistJobInfo <- getPersistJobInfo _jobId
      return $ pendingQuery (entityKey query) $ QIRJobInfo mPersistJobInfo
    Nothing -> do
      mKey <- insertQuery q
      runBaseConcurrent $ do
        con <- getConnection
        mJobInfo <- getJobInfo con _jobId
        case mJobInfo of
          Nothing -> deleteQuery q
          Just ji -> do
            currentTime <- liftIO getCurrentTime
            _ <- runDB $ do
              deleteBy $ UniqueJobInfo _jobId
              insertUnique $ JobInfo _jobId
                                     (jobInfoName ji)
                                     (jobInfoStatus ji)
                                     (jobInfoDate ji)
                                     currentTime
            deleteQuery q
            liftIO $ putStrLn $ "Job done: " ++ (show q)
      return $ pendingQuery (fromJust mKey) $ QIRJobInfo Nothing
runQuery q@(GetSolverInfo _solverId) = undefined
runQuery q@(GetBenchmarkInfo _benchId) = undefined
runQuery q@(GetJobPair _pairId) = undefined
runQuery q@(GetJobResults _jobId) = do
  mQuery <- getQuery q
  case mQuery of
    Just query -> do
      mPersistJobResults <- getPersistJobResults _jobId
      return $ pendingQuery (entityKey query) $ QIRJobResults mPersistJobResults
    Nothing -> do
      mKey <- insertQuery q
      runBaseConcurrent $ do
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
      return $ pendingQuery (fromJust mKey) $ QIRJobResults []
