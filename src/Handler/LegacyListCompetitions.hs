module Handler.LegacyListCompetitions where

import Import
import Network.HTTP.Types.Status

getLegacyListCompetitionsR :: Handler Html
getLegacyListCompetitionsR = redirectWith movedPermanently301 ListCompetitionsR
