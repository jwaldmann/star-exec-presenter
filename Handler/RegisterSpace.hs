module Handler.RegisterSpace where

import Import
import qualified StarExec.Commands as SEC

updateSpace spaceID = do
    space <- runDB $ getBy $ UniqueSpace spaceID
    _ <- case space of
        Nothing -> runDB $ insertUnique $ Space spaceID []
        Just sid -> runDB $ return Nothing
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
