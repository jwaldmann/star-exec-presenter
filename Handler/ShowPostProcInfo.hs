module Handler.ShowPostProcInfo where

import Import
import StarExec.JobData
import StarExec.Types
import Utils.WidgetMetaRefresh

getShowPostProcInfoR :: Int -> Handler Html
getShowPostProcInfoR _procId = do
  (QueryResult qStatus mPostProcInfo) <- queryPostProc _procId
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "show_post_proc_info")
