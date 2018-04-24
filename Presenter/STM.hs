module Presenter.STM
  ( startWorker
  , lookupCache
  ) where

import Import

import Presenter.CompetitionResults

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Concurrent ( threadDelay )
import Control.Exception.Base
import Control.Monad ( when )
import Control.Monad.Logger

-- | slightly shorter than the display refresh rate
defaultWorkerDelay :: Int
defaultWorkerDelay = 1 * 10^6

{- caching mechanism:

run-time (in-memory) cache is a TVar
that holds a map from competition ids to TVars with results.

-}

-- | if competition is in run-time cache, then read its contents.
-- else start worker and return empty contents.
lookupCache :: Competition -> Handler (Maybe CompetitionResults)
lookupCache comp = do
  app <- getYesod
  (mCompResults, start) <- liftIO $ atomically $ do
    crc <- readTVar $ compResultsCache app
    case M.lookup (getMetaData comp) crc of
        Nothing -> do
            return (Nothing, True)
        Just entry -> do
            mCompResults <- readTVar entry
            return (mCompResults, False)
  when start $ startWorker comp
  return mCompResults

-- | starting a worker for a competition that is not yet in the run-time cache.
-- 
startWorker :: Competition -> Handler ()
startWorker comp = do
  app <- getYesod
  mSink <- liftIO $ atomically $ do
    crc <- readTVar $ compResultsCache app
    case M.lookup (getMetaData comp) crc of
      Nothing -> do
          sink <- newTVar Nothing
          modifyTVar' (compResultsCache app) $ M.insert (getMetaData comp) sink
          return $ Just sink
      Just _ -> do
          return Nothing
  case mSink of
    Nothing -> return ()
    Just sink -> do
      logInfoN $ T.pack $ "start worker for " ++ ( T.unpack $ getCompetitionName comp )
      forkHandler errHandler runWorker
      where
        errHandler e = do
          exceptionHandler e
          logWarnN $  T.pack $ "restarting worker:"
          forkHandler errHandler runWorker
        runWorker = do
          logWarnN $  T.pack $ "runWorker: getCompetitionResults."
          compResults <- getCompetitionResults comp
          logWarnN $  T.pack $ "runWorker: got CompetitionResults."
          liftIO $ atomically $ writeTVar sink $ Just compResults
          liftIO $ threadDelay defaultWorkerDelay
          when (not $ competitionComplete compResults) runWorker

exceptionHandler :: SomeException -> Handler ()
exceptionHandler e = do
  logWarnN $ T.pack $ "ignoring exception:" <> show e
