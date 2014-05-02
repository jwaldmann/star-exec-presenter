module Handler.User where

import Import

getUserR :: Handler Html
getUserR = do
    defaultLayout $ do
        $(widgetFile "user")
