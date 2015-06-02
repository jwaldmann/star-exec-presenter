module Handler.ListJobPairs where

import Import
import Presenter.PersistHelper

-- Pagination is definitly needed!

getAll :: Handler [JobResult]
getAll = runDB $ do
  starExecResults <- do
    results <- getEntityList' ([] :: [Filter JobResultInfo]) []
    return $ StarExecResult <$> results
  lriResults <- do
    results <- getEntityList' ([] :: [Filter LriResultInfo]) []
    return $ LriResult <$> results
  return $ starExecResults ++ lriResults

getListJobPairsR :: Handler Html
getListJobPairsR = do
  results <- getAll
  defaultLayout $ do
    $(widgetFile "list_job_pairs")
