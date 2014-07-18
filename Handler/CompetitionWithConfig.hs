module Handler.CompetitionWithConfig where

import Import
import StarExec.Types
import StarExec.CompetitionResults
import StarExec.JobData
import Utils.WidgetMetaRefresh
import Data.Time.Clock

import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Handler.Control (start_worker)
import Control.Monad ( when )

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
  (mCompResults, start) <- lift $ atomically $ do
      crc <- readTVar $ compResultsCache app
      case M.lookup (getCompetitionName comp) crc of
          Nothing -> do
              return (Nothing, True)
          Just entry -> do
              mCompResults <- readTVar entry
              return (mCompResults, False)
  when start $ start_worker comp

  let need_refresh = case mCompResults of
          Nothing -> True
          Just cr -> not $ competitionComplete cr
  
  defaultLayout $ do
    if -- any (\q -> queryStatus q /= Latest) qJobs
      need_refresh
      then insertWidgetMetaRefresh
      else return ()
    case mCompResults of
        Nothing -> [whamlet|competition currently not in results cache|]
        Just compResults -> $(widgetFile "competition_slim")
