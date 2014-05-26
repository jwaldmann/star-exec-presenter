{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified StarExec.Commands as SEC
import StarExec.Session

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        con <- SEC.getConnection
        loggedIn <- hasValidSession
        mUserID <- do
            if loggedIn
                then getSessionUserID
                else return Nothing
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
