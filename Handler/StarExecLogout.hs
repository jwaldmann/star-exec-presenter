module Handler.StarExecLogout where

import Import
import qualified StarExec.Commands as SEC

getStarExecLogoutR :: Handler Html
getStarExecLogoutR = do
    con <- SEC.getConnection
    SEC.logout con
    redirect HomeR
