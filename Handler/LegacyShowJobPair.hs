module Handler.LegacyShowJobPair where

import Import
import Handler.ShowJobPair

getLegacyShowJobPairR :: JobPairID -> Handler Html
getLegacyShowJobPairR = getShowJobPairR
