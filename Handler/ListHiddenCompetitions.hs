module Handler.ListHiddenCompetitions where

import Import
import Yesod.Auth
import Data.Maybe
import Presenter.PersistHelper

getListHiddenCompetitionsR :: Handler Html
getListHiddenCompetitionsR = do
  maid <- maybeAuthId
  let authorized = isJust maid
  competitionInfos <- getPersistCompetitions
  defaultLayout $ do
    $(widgetFile "list_competitions")
