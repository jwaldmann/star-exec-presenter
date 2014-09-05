module Handler.LegacyShowPostProcInfo where

import Import
import Handler.ShowPostProcInfo

getLegacyShowPostProcInfoR :: Int -> Handler Html
getLegacyShowPostProcInfoR = getShowPostProcInfoR
