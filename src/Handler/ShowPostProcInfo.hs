module Handler.ShowPostProcInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Utils.WidgetMetaRefresh

getShowPostProcInfoR :: PostProcID -> Handler Html
getShowPostProcInfoR (StarExecPostProcID _procId) = do
  (QueryResult qStatus mPostProcInfo) <- queryPostProc _procId
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "show_post_proc_info")
