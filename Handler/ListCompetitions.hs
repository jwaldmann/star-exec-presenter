module Handler.ListCompetitions where

import Import
import StarExec.Persist
import StarExec.Types

getListCompetitionsR :: Handler Html
getListCompetitionsR = do
  competitionInfos <- getPersistPublicCompetitions
  defaultLayout $ do
    $(widgetFile "list_competitions")
