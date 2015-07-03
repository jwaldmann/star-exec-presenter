module Handler.ListCompetitions where

import Import
import Presenter.PersistHelper

getListCompetitionsR :: Handler Html
getListCompetitionsR = do
  competitionInfos <- getPersistPublicCompetitions
  defaultLayout $ do
    $(widgetFile "list_competitions")
