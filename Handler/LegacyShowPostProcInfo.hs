module Handler.LegacyShowPostProcInfo where

import Import
import Network.HTTP.Types.Status

getLegacyShowPostProcInfoR :: PostProcID -> Handler Html
getLegacyShowPostProcInfoR = (redirectWith movedPermanently301) . ShowPostProcInfoR
