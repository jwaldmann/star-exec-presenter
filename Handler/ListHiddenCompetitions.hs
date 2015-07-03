module Handler.ListHiddenCompetitions where

import Import
import Presenter.PersistHelper

getListHiddenCompetitionsR :: Handler Html
getListHiddenCompetitionsR = do
  competitionInfos <- getPersistCompetitions
  defaultLayout $ do
    $(widgetFile "list_competitions")
