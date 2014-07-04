module Handler.ShowJobPair where

import Import
import StarExec.Types
import StarExec.JobData
import StarExec.Persist

getShowJobPairR :: Int -> Handler Html
getShowJobPairR _pairId = do
  (QueryResult qStatus mPair) <- queryJobPair _pairId
  defaultLayout $ do
    $(widgetFile "show_job_pair")
