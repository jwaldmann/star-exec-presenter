module Handler.CompetitionText where

import Import
import Yesod.Auth
import Data.Maybe

import Presenter.Output

getCompetitionTextR :: CompetitionInfoId -> Handler Html
getCompetitionTextR compId = do
    mCompInfo <- runDB $ get compId
    defaultLayout [whamlet|
<h1>Source Code for CompetitionInfo #{show compId}
$maybe ci <- mCompInfo
  <pre>#{show $ output $ competitionInfoCompetition ci}
$nothing
  none found
|]                   
