module Handler.ShowJobInfo where

import Import
import StarExec.JobData
import StarExec.Types

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR _jobId = do
  (QueryResult qStatus mJobInfo) <- queryJobInfo _jobId
  defaultLayout $ do
    $(widgetFile "show_job_info")

