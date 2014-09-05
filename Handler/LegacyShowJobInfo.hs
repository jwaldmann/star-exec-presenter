module Handler.LegacyShowJobInfo where

import Import
import Handler.ShowJobInfo

getLegacyShowJobInfoR :: JobID -> Handler Html
getLegacyShowJobInfoR = getShowJobInfoR
