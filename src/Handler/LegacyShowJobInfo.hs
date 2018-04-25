module Handler.LegacyShowJobInfo where

import Import
import Network.HTTP.Types.Status

getLegacyShowJobInfoR :: JobID -> Handler Html
getLegacyShowJobInfoR = (redirectWith movedPermanently301) . ShowJobInfoR
