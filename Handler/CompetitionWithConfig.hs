module Handler.CompetitionWithConfig where

import Import
import StarExec.Types
import StarExec.CompetitionResults
import StarExec.CompetitionResults.Type
import StarExec.JobData
import StarExec.STM
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

  mCompResults <- lookupCache comp

  let need_refresh = case mCompResults of
          Nothing -> True
          Just cr -> not $ competitionComplete cr
  
  defaultLayout $ do
    if need_refresh
      then insertWidgetMetaRefresh
      else return ()
    case mCompResults of
        Nothing -> [whamlet|competition currently not in results cache|]
        Just compResults -> $(widgetFile "competition_slim")
