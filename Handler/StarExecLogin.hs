module Handler.StarExecLogin where

import Import
import qualified StarExec.Commands as SEC
import StarExec.ErrorID

getStarExecLoginR :: Handler Html
getStarExecLoginR = redirect HomeR

postStarExecLoginR :: Handler ()
postStarExecLoginR = do
    (email, pass) <- runInputPost $ (,) <$> ireq textField "email" <*> ireq textField "password"
    con <- SEC.getConnection
    loggedIn <- SEC.login con email pass
    if loggedIn
        then redirect HomeR
        else redirect $ ErrorR Login
