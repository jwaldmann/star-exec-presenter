module Handler.FlexibleTable where

import Import
import Handler.ShowManyJobResults

getFlexibleTableR :: Query -> JobIds -> Handler Html
getFlexibleTableR = getShowManyJobResultsR
