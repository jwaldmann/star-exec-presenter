module Handler.CompetitionWithConfig where

import Import
import StarExec.Types
import StarExec.CompetitionResults

getCompetitionWithConfigR :: Competition -> Handler Html
getCompetitionWithConfigR comp = do
  let compName = getCompetitionName comp
  compResults <- getCompetitionResults comp
  defaultLayout $ do
    $(widgetFile "competition")
