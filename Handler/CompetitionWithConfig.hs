module Handler.CompetitionWithConfig where

import Import
import StarExec.Types
import StarExec.CompetitionResults
import StarExec.JobData
import Utils.WidgetMetaRefresh
import Data.Time.Clock

minute :: Seconds
minute = 60

hour :: Seconds
hour = 60 * minute

getDuration :: Maybe UTCTime -> Maybe UTCTime -> String
getDuration Nothing _ = "unkown time"
getDuration _ Nothing = "unkown time"
getDuration (Just start) (Just end) =
  let duration = diffTime end start
  in renderTime duration
  where
    renderTime d =
      if d >= hour
        then let hours = floor $ d / hour
             in (show hours) ++ "h " ++ renderTime ( d - (fromIntegral hours) * hour )
        else if d > minute
          then let minutes = floor $ d / minute
               in (show minutes) ++ "m " ++ renderTime ( d - (fromIntegral minutes) * minute )
          else (show $ floor d) ++ "s"

getCompetitionWithConfigR :: Competition -> Handler Html
getCompetitionWithConfigR comp = do

  app <- getYesod

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
