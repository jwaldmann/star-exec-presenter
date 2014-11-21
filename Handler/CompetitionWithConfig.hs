module Handler.CompetitionWithConfig where

import Import
import Presenter.STM
import Presenter.Utils.WidgetMetaRefresh
import Presenter.Prelude
import Data.Time.Clock

minute :: Seconds
minute = 60

hour :: Seconds
hour = 60 * minute

getDuration :: Maybe UTCTime -> Maybe UTCTime -> String
getDuration mStart mEnd =
  case diffTime <$> mEnd <*> mStart of
    Nothing -> "unkown time"
    Just d  -> renderTime d
  where
    renderTime d =
      if d >= hour
        then let hours = floor $ d / hour
             in (show hours) ++ "h " ++ renderTime ( d - (fromIntegral hours) * hour )
        else if d > minute
          then let minutes = floor $ d / minute
               in (show minutes) ++ "m " ++ renderTime ( d - (fromIntegral minutes) * minute )
          else (show $ floor d) ++ "s"

getCompletionClass :: Bool -> Text
getCompletionClass True = "completed"
getCompletionClass False = "running"

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
        Just compResults -> $(widgetFile "competition_slim2")
