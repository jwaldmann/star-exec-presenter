module Handler.LegacyShowJobResults where

import Import
import Network.HTTP.Types.Status

getLegacyShowJobResultsR :: JobIds -> Handler Html
getLegacyShowJobResultsR = (redirectWith movedPermanently301) . ShowManyJobResultsR NoQuery
