module Handler.LegacyShowManyJobResults where

import Import
import Network.HTTP.Types.Status

getLegacyShowManyJobResultsR :: JobIds -> Handler Html
getLegacyShowManyJobResultsR = (redirectWith movedPermanently301) . ShowManyJobResultsR NoQuery
