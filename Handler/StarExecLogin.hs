module Handler.StarExecLogin where

import Import
import qualified StarExec.Commands as SEC
import StarExec.ErrorID
import Data.Maybe

updateUser userID = do
    _ <- runDB $ insertUnique $ User userID []
    --user <- runDB $ getBy $ UniqueUser userID
    --_ <- case user of
    --        Nothing -> runDB $ insertUnique $ User userID []
    --        Just uid -> runDB $ return Nothing
    return ()

getStarExecLoginR :: Handler Html
getStarExecLoginR = do
    redirect HomeR

postStarExecLoginR :: Handler ()
postStarExecLoginR = do
    (email, pass) <- runInputPost $ (,) <$> ireq textField "email" <*> ireq textField "password"
    con <- SEC.getConnection
    loggedIn <- SEC.login con email pass
    if loggedIn
        then do
            userID <- SEC.getSessionUserID
            case userID of
                Nothing -> return ()
                Just uid -> updateUser uid
            redirect HomeR
        else redirect $ ErrorR Login
