module Handler.ShowManyJobResults where

import Import
import StarExec.Types

getShowManyJobResultsR :: JobIds -> Handler Html
getShowManyJobResultsR (JobIds ids) = error $ show ids
