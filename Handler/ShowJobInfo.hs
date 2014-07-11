module Handler.ShowJobInfo where

import Import
import StarExec.JobData
import StarExec.Types
import Utils.WidgetMetaRefresh

getShowJobInfoR :: Int -> Handler Html
getShowJobInfoR _jobId = do
  (QueryResult qStatus mJobInfo) <- queryJobInfo _jobId
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "show_job_info")

