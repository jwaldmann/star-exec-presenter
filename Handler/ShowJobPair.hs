module Handler.ShowJobPair where

import Import
import StarExec.Persist
import StarExec.Types

getShowJobPairR :: Int -> Handler Html
getShowJobPairR _pairId = do
  mPair <- getJobPair _pairId
  defaultLayout $ do
    $(widgetFile "show_job_pair")
