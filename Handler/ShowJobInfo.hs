module Handler.ShowJobInfo where

import Import
import StarExec.Connection
import StarExec.Commands

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR _jobId = do
  con <- getConnection
  mJobInfo <- getJobInfo con _jobId
  defaultLayout $ do
    $(widgetFile "show_job_info")

