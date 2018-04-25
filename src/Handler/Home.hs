module Handler.Home where

import Import
import Yesod.Auth
import Data.Maybe

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    let authorized = isJust maid
    defaultLayout $ do
        setTitle "Welcome to Star-Exec-Presenter"
        $(widgetFile "homepage")
