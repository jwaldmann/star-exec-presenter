module Handler.CompetitionYear where

import Import
import Handler.CompetitionWithConfig
import Presenter.History
import Yesod.Auth
import Data.Maybe

import Control.Monad.Logger
import qualified Data.Text as T

getCompetitionYearR :: Year -> Handler Html
getCompetitionYearR year = do
  logWarnN $ T.pack $ "getCompetitionYearR" <> show year
  case info_for_year year of
    Just ci -> do
       getCompetitionWithConfigR  ci
    Nothing -> notFound

