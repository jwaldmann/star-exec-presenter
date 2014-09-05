module Handler.LegacyShowManyJobResults where

import Import
import Handler.ShowManyJobResults

getLegacyShowManyJobResultsR :: JobIds -> Handler Html
getLegacyShowManyJobResultsR = getShowManyJobResultsR NoQuery
