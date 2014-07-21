module Handler.Competition where

import Import
import Yesod.Auth
import StarExec.Types
import Handler.CompetitionWithConfig
import Data.Maybe (isJust)


getCompetitionR :: CompetitionInfoId -> Handler Html
getCompetitionR compId = do
  compInfo <- runDB $ get compId
  case compInfo of
    Just ci -> do
        maid <- maybeAuthId
        if competitionInfoPublic ci || isJust maid 
           then getCompetitionWithConfigR $ competitionInfoCompetition ci
           else defaultLayout [whamlet|this competition is not public, and you are not authorized to view it|]
    Nothing -> notFound
