module Handler.ShowJobInfo where

import Import
import Presenter.StarExec.JobData
import Presenter.Internal.Stringish
import Presenter.Utils.WidgetMetaRefresh
import Presenter.PersistHelper

getStarExecDate :: Job -> Text
getStarExecDate (StarExecJob j) = jobInfoDate j
getStarExecDate _ = notAvailable

getPreProc :: Job -> Text
getPreProc (StarExecJob j) = jobInfoPreProc j
getPreProc _ = notAvailable

getPostProc :: Job -> Text
getPostProc (StarExecJob j) = jobInfoPostProc j
getPostProc _ = notAvailable

isComplexity_ :: Job -> Text
isComplexity_ j = fromBool $ isComplexity j

getStartDate :: Job -> Text
getStartDate (StarExecJob j) = fromString $ show $ jobInfoStartDate j
getStartDate _ = notAvailable

getFinishDate :: Job -> Maybe Text
getFinishDate (StarExecJob j) = (fromString . show) <$> jobInfoFinishDate j
getFinishDate _ = Just notAvailable

getLastUpdate :: Job -> Text
getLastUpdate (StarExecJob j) = fromString $ show $ jobInfoLastUpdate j
getLastUpdate _ = notAvailable

getShowJobInfoR :: JobID -> Handler Html
getShowJobInfoR jid@(StarExecJobID _id) = do
  (QueryResult qStatus (mJobInfo,_)) <- queryJob jid
  defaultLayout $ do
    case qStatus of
      Latest -> return ()
      Pending _ -> insertWidgetMetaRefresh
    $(widgetFile "se_show_job_info")
getShowJobInfoR jid@(LriJobID _id) = do
  mJobInfo <- getPersistJobInfo jid
  defaultLayout $ do
    $(widgetFile "lri_show_job_info")
