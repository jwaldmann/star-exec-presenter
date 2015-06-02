module Handler.LegacyShowJobPair where

import Import
import Network.HTTP.Types.Status

getLegacyShowJobPairR :: JobPairID -> Handler Html
getLegacyShowJobPairR = (redirectWith movedPermanently301) . ShowJobPairR
