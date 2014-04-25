{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Network.HTTP.Conduit
import qualified StarExec.StarExecCommands as SEC

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
        loggedIn <- lookupSession "SESSION" >>= SEC.checkLogin
        aDomId <- newIdent
        sess <- getSession
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

--postHomeR :: Handler Html
--postHomeR = do
--    (email, pass) <- runInputPost $ (,) <$> ireq textField "email" <*> ireq textField "password"
--    session <- lookupSession "SESSION"
--    cookies <- SEC.login email pass session
--    setSession "SESSION" cookies
--    redirect HomeR

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
