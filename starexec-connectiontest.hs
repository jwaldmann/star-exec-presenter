import Presenter.StarExec.Urls
import Presenter.Auth (getLoginCredentials)

import Presenter.Model.Types ( user, password )
import System.Directory ( getHomeDirectory )
import qualified Data.Text.Encoding as TE

import Network.Connection
import qualified Network.HTTP.Client.Conduit as NHCC
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (ok200)
import qualified Network.HTTP.Types.Header as H
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar

main = do
  let cj0 = createCookieJar [ killmenothing ]
      Just base = parseUrl starExecUrl
  man <- NHCC.newManagerSettings
      $ ( mkManagerSettings ( TLSSettingsSimple True False False ) Nothing )
      { managerResponseTimeout = Just $ 60 * 10^6
      , managerConnCount = 10
      } 

  let send req = do
        print req
        resp <- httpLbs req man 
        print resp
        return resp
  
  resp1 <- send
      $ base { method = "GET", path = indexPath
             , cookieJar = Just cj0
             , redirectCount = 0 }
  let cj1 = responseCookieJar resp1

  creds <- getLoginCredentials
  resp2 <- send
           $ urlEncodedBody [ ("j_username", TE.encodeUtf8 $ user creds)
                           , ("j_password", TE.encodeUtf8 $ password creds) 
                           , ("cookieexists", "false")
                           ] 
                 $ base { method = "POST" , path = loginPath
                        , cookieJar = Just cj1
                        , redirectCount = 2
                        }

  let cj2 = responseCookieJar resp2
  resp3 <- send $ base { method = "GET", path = indexPath, redirectCount = 1
                       , cookieJar = Just cj2
                       }

  

  return ()



killmenothing :: Cookie
killmenothing = Cookie { cookie_name = "killmenothing"
                , cookie_value = ""
                , cookie_expiry_time = future
                , cookie_domain = "www.starexec.org"
                , cookie_path = "/starexec/"
                , cookie_creation_time = past
                , cookie_last_access_time = past
                , cookie_persistent = False
                , cookie_host_only = True
                , cookie_secure_only = True
                , cookie_http_only = True
                }

past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)
