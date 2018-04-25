module Handler.ListCompetitions where

import Import
import Yesod.Auth
import Data.Maybe
import Presenter.PersistHelper

getListCompetitionsR :: Handler Html
getListCompetitionsR = do
  maid <- maybeAuthId
  let authorized = isJust maid
  competitionInfos <- getPersistPublicCompetitions
  defaultLayout $ do
    $(widgetFile "list_competitions")
