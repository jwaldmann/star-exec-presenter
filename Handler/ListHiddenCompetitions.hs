module Handler.ListHiddenCompetitions where

import Import
import StarExec.Persist
import StarExec.Types

getListHiddenCompetitionsR :: Handler Html
getListHiddenCompetitionsR = do
  competitionInfos <- getPersistCompetitions
  defaultLayout $ do
    $(widgetFile "list_competitions")
