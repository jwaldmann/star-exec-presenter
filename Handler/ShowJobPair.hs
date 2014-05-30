module Handler.ShowJobPair where

import Import

getShowJobPairR :: Int -> Handler Html
getShowJobPairR _pairId = do

  defaultLayout $ do
    $(widgetFile "show_job_pair")
