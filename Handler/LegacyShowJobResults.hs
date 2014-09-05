module Handler.LegacyShowJobResults where

import Import
import Handler.ShowManyJobResults

getLegacyShowJobResultsR :: JobIds -> Handler Html
getLegacyShowJobResultsR = getShowManyJobResultsR NoQuery
