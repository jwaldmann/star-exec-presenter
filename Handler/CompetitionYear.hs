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
  let compInfo = case year of
        Y2014 -> Just termcomp2014
        Y2015 -> Just termcomp2015
        Y2016 -> Just termcomp2016
        _     -> Nothing

  case compInfo of
    Just ci -> do
       getCompetitionWithConfigR  ci
    Nothing -> notFound

