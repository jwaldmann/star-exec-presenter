module Handler.LegacyShowPostProcInfo where

import Import
import Handler.ShowPostProcInfo

getLegacyShowPostProcInfoR :: PostProcID -> Handler Html
getLegacyShowPostProcInfoR = getShowPostProcInfoR
