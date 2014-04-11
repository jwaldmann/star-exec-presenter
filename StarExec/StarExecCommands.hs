module StarExec.StarExecCommands
    ( index
    , login
    , logout
    ) where

import Import
import Network.HTTP.Conduit
-- import Control.Monad.IO.Class
import qualified Data.ByteString as BS

-- Static-Paths

starExecUrl :: String
starExecUrl = "https://www.starexec.org/"

indexPath :: BS.ByteString
indexPath = "starexec/secure/index.jsp"

loginPath :: BS.ByteString
loginPath = "starexec/secure/j_security_check"

logoutPath :: BS.ByteString
logoutPath = "starexec/services/session/logout"

index :: MonadIO m => Request -> Manager -> CookieJar -> m CookieJar
index sec man cookies = do
    let req = sec { method = "GET"
                  , path = indexPath
                  , cookieJar = Just cookies
                  }
    resp <- httpLbs req man
    let respCookies = responseCookieJar resp
    return respCookies

login :: MonadIO m => BS.ByteString -> BS.ByteString -> Request -> Manager -> CookieJar -> m CookieJar
login user pass sec man cookies = do
    let req = urlEncodedBody [ ("j_username", user)
                             , ("j_password",  pass) 
                             , ("cookieexists", "false")
                             ] 
                $ sec { method = "POST"
                      , path = loginPath
                      , cookieJar = Just $ cookies
                      }
    resp <- httpLbs req man
    let respCookies = responseCookieJar resp
    return respCookies

logout :: MonadIO m => Request -> Manager -> CookieJar -> m CookieJar
logout sec man cookies = do
    let req = sec { method = "POST"
                  , path = logoutPath
                  , cookieJar = Just cookies
                  }
    resp <- httpLbs req man
    let respCookies = responseCookieJar resp
    return respCookies
