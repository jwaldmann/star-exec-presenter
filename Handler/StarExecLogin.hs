module Handler.StarExecLogin where

import Import
import qualified StarExec.Commands as SEC
import StarExec.ErrorID
import Data.Maybe

--updateUser userID = do
--    user <- runDB $ getBy $ UniqueUser userID
--    case user of
--            Nothing -> runDB $ insertUnique $ User userID []
--            Just u -> return Nothing

getStarExecLoginR :: Handler Html
getStarExecLoginR = do
    redirect HomeR

postStarExecLoginR :: Handler ()
postStarExecLoginR = do
    (email, pass) <- runInputPost $ (,) <$> ireq textField "email" <*> ireq textField "password"
    con <- SEC.getConnection
    loggedIn <- SEC.login con email pass
    --liftIO $ print $ "logged in" ++ ( show loggedIn )
    --redirect HomeR
    if loggedIn
        then redirect HomeR
        else redirect $ ErrorR Login
