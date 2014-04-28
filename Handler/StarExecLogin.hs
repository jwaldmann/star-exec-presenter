module Handler.StarExecLogin where

import Import
import qualified StarExec.StarExecCommands as SEC

postStarExecLoginR :: Handler Html
postStarExecLoginR = do
    (email, pass) <- runInputPost $ (,) <$> ireq textField "email" <*> ireq textField "password"
    con <- SEC.getConnection
    cookies <- SEC.login con email pass
    redirect HomeR
