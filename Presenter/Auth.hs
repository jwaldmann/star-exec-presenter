-- | authentication relative to the data in $HOME/.star_exec

module Presenter.Auth where

-- source copied from 
-- http://hackage.haskell.org/package/yesod-auth-1.3.1.1/docs/src/Yesod-Auth-Dummy.html
-- and modified

import Prelude
import Yesod.Auth
import Yesod.Form (runInputPost, textField, ireq)
import Text.Hamlet (hamlet)
import Yesod.Core

import Presenter.Model.Types ( Login (..))
import System.Directory ( getHomeDirectory )
import qualified Yesod.Auth.Message as Msg

-- moved here to break cyclic import
-- getLoginCredentials :: Handler Login
getLoginCredentials :: (MonadIO m, Read a) => m a
getLoginCredentials = liftIO $ do
  home <- getHomeDirectory
  slogin <- readFile $ home ++ "/.star_exec"
  return $ read slogin


authSE :: YesodAuth m => AuthPlugin m
authSE =
    AuthPlugin "authSE" dispatch login
  where
    dispatch "POST" [] = do
        ident <- lift $ runInputPost $ ireq textField "ident"
        pass  <- lift $ runInputPost $ ireq textField "pass"
        cred <- lift $ getLoginCredentials
        if cred == Login ident pass
                then lift $ setCredsRedirect $ Creds "authSE" ident []
                else loginErrorMessageI LoginR Msg.PassMismatch
    dispatch _ _ = notFound
    url = PluginR "authSE" []
    login authToMaster =
        toWidget [hamlet|
$newline never
<form method="post" action="@{authToMaster url}">
    <p>prove that you are the person who started this instance of star-exec-presenter: 
    <p>
        <input type="text" name="ident">
        <input type="password" name="pass">
        <input type="submit" value="prove">
|]
