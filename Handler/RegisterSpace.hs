module Handler.RegisterSpace where

import Import
import qualified StarExec.Commands as SEC

updateSpace spaceID = do
    _ <- runDB $ insertUnique $ Space spaceID []
    return ()

postRegisterSpaceR :: Handler Html
postRegisterSpaceR = do
    spaceID <- runInputPost $ ireq textField "spaceId"
    con <- SEC.getConnection
    loggedIn <- SEC.checkLogin con
    _ <- if loggedIn
            then updateSpace spaceID
            else return ()
    redirect UserR
