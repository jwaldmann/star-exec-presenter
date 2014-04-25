module Handler.StarExecLogin where

import Import
import qualified StarExec.StarExecCommands as SEC

postStarExecLoginR :: Handler Html
postStarExecLoginR = do
    (email, pass) <- runInputPost $ (,) <$> ireq textField "email" <*> ireq textField "password"
    session <- lookupSession "SESSION"
    cookies <- SEC.login email pass session
    setSession "SESSION" cookies
    redirect HomeR
