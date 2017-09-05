module Handler.CompetitionYear where

import Import
import Handler.CompetitionWithConfig
import Presenter.History
import Yesod.Auth
import Data.Maybe

import Control.Monad.Logger
import qualified Data.Text as T

info_for_year :: Year -> Maybe Competition
info_for_year year = case year of
  Y2014 -> Just termcomp2014
  Y2015 -> Just termcomp2015
  Y2016 -> Just termcomp2016
  Y2017 -> Just termcomp2017
  _     -> Nothing 

getCompetitionYearR :: Year -> Handler Html
getCompetitionYearR year = do
  logWarnN $ T.pack $ "getCompetitionYearR" <> show year
  case info_for_year year of
    Just ci -> do
       getCompetitionWithConfigR  ci
    Nothing -> notFound

