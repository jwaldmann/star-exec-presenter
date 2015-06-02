module Handler.ListJobs where

import Import
import Presenter.PersistHelper

getAll :: Handler [Job]
getAll = runDB $ do
  starExecJobs <- do
    jobs <- getEntityList' ([] :: [Filter JobInfo]) []
    return $ StarExecJob <$> jobs
  lriJobs <- do
    jobs <- getEntityList' ([] :: [Filter LriJobInfo]) []
    return $ LriJob <$> jobs
  return $ starExecJobs ++ lriJobs

getListJobsR :: Handler Html
getListJobsR = do
  jobs <- getAll
  defaultLayout $ do
    $(widgetFile "list_jobs")
