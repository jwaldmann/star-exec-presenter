module Handler.CompetitionWithConfig where

import Import
import StarExec.Types
import StarExec.CompetitionResults
import StarExec.JobData
import Utils.WidgetMetaRefresh

getCompetitionWithConfigR :: Competition -> Handler Html
getCompetitionWithConfigR comp = do
  let metaCats = getMetaCategories comp
      cats = concat $ map getCategories metaCats
      jobIds = concat $ map getJobIds cats
  qJobs <- queryManyJobs jobIds
  compResults <- getCompetitionResults comp
  defaultLayout $ do
    if any (\q -> queryStatus q /= Latest) qJobs
      then insertWidgetMetaRefresh
      else return ()
    $(widgetFile "competition_slim")
