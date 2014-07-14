module Handler.ListHiddenCompetitions where

import Import
import StarExec.Persist
import StarExec.Types

getListHiddenCompetitionsR :: Handler Html
getListHiddenCompetitionsR = do
  competitionInfos <- getPersistCompetitions
  let competitions = map competitionInfoCompetition competitionInfos
  defaultLayout $ do
    $(widgetFile "list_competitions")
