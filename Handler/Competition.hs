module Handler.Competition where

import Import
import Handler.CompetitionWithConfig
import Handler.CompetitionYear
import Yesod.Auth
import Data.Maybe

import Control.Monad.Logger
import qualified Data.Text as T

getCompetitionR :: CompetitionRef -> Handler Html
getCompetitionR cr = case cr of
  CRefId i -> getCompetitionByIdR i
  CRefYear y -> getCompetitionYearR y

getCompetitionByIdR :: CompetitionInfoId -> Handler Html
getCompetitionByIdR compId = do
  logWarnN $ T.pack $ "getCompetitionByIdR" <> show compId
  compInfo <- runDB $ get compId

  case compInfo of
    Just ci -> do
        maid <- maybeAuthId
        if competitionInfoPublic ci || isJust maid
           then getCompetitionWithConfigR $ competitionInfoCompetition ci
           else defaultLayout [whamlet|this competition is not public, and you are not authorized to view it|]
    Nothing -> notFound
