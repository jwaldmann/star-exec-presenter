module Handler.Competition where

import Import
import StarExec.Types
import Handler.CompetitionWithConfig

getCompetitionR :: CompetitionInfoId -> Handler Html
getCompetitionR compId = do
  compInfo <- runDB $ get compId
  case compInfo of
    Just ci -> getCompetitionWithConfigR $ competitionInfoCompetition ci
    Nothing -> notFound
