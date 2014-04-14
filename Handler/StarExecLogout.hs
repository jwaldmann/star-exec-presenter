module Handler.StarExecLogout where

import Import
import qualified StarExec.StarExecCommands as SEC

getStarExecLogoutR :: Handler Html
getStarExecLogoutR = do
    session <- lookupSession "SESSION"
    cookies <- SEC.logout session
    deleteSession "SESSION"
    redirect HomeR
