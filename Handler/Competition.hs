module Handler.Competition where

import Import
import Handler.CompetitionWithConfig
import Yesod.Auth
import Data.Maybe

import Control.Monad.Logger
import qualified Data.Text as T

getCompetitionR :: CompetitionInfoId -> Handler Html
getCompetitionR compId = do
  logWarnN $ T.pack $ "getCompetitionR" <> show compId
  compInfo <- runDB $ get compId
  logWarnN $ T.pack $ "getCompetitionR" <> show compInfo

  case compInfo of
    Just ci -> do
        maid <- maybeAuthId
        if competitionInfoPublic ci || isJust maid
           then getCompetitionWithConfigR $ competitionInfoCompetition ci
           else defaultLayout [whamlet|this competition is not public, and you are not authorized to view it|]
    Nothing -> notFound
