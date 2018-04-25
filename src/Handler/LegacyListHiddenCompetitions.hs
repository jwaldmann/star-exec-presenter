module Handler.LegacyListHiddenCompetitions where

import Import
import Network.HTTP.Types.Status

getLegacyListHiddenCompetitionsR :: Handler Html
getLegacyListHiddenCompetitionsR = redirectWith movedPermanently301 ListHiddenCompetitionsR
