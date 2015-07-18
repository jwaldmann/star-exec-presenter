module Presenter.StarExec.Connection
    ( sendRequest
    -- , index
    , getLoginCredentials
    , getConnection -- this is used (only) by Presenter.Control.Job (in pushJobXml)
    ) where

import Import
import Prelude (head)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (ok200)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Presenter.StarExec.Urls
import Presenter.Auth ( getLoginCredentials )
import Presenter.Prelude (diffTime)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Control.Concurrent.STM
import Control.Concurrent.MVar
-- import Control.Concurrent.SSem
import qualified Control.Concurrent.FairRWLock as Lock
import Control.Exception (throw)
import Control.Monad.Catch (bracket_)
import Control.Monad ((>=>))
import Control.Monad.Logger


user :: Login -> Text
user (Login u _) = u

password :: Login -> Text
password (Login _ pass) = pass

runCon_exclusive :: Handler b -> Handler b
runCon_exclusive action = do
  lock <- conSem <$> getYesod 
  -- Lock.withWrite lock action
  bracket_
    ( lift $ Lock.acquireWrite lock )
    ( lift $ (Lock.releaseWrite >=> either throw return) lock)
    action

getSessionData :: Handler SessionData
getSessionData = do
  app <- getYesod
  lift $ atomically $ readTVar $ sessionData app

setSessionData :: CookieJar -> UTCTime -> Handler ()
setSessionData cj d = do
  app <- getYesod
  lift $ atomically $ writeTVar (sessionData app) $ SessionData cj d

-- | raw request. May return "Login" response if we're not currently logged in.
-- will silently set cookies to session state.
sendRequestRaw :: StarExecConnection
               -> Handler (Response BSL.ByteString)
sendRequestRaw (req, man, _ ) = do
  SessionData cj d <- getSessionData
  let req' =  req { cookieJar = Just cj }
  logWarnN  $ T.pack  $ "sendRequestRaw: " <> show req
  resp <- httpLbs req' man
  logWarnN  $ T.pack  $ "done sendRequestRaw: " <> show req
                       <> "response status: " <> show (responseStatus resp)
  now <- liftIO getCurrentTime
  setSessionData (responseCookieJar resp) now
  return resp

-- | managed requests: will execute Login if necessary.
sendRequest (req0, man, _ ) = runCon_exclusive $ do
  logWarnN  $ T.pack  $ "sendRequest: " <> show req0
  resp0 <- sendRequestRaw (req0 { checkStatus = \ _ _ _ -> Nothing }, man, undefined )
  if not $ needs_login resp0
     then do
       logWarnN  $ T.pack  $ "sendRequest: OK"
       return resp0
    else do    
       logWarnN  $ T.pack  $ "sendRequest: not OK, need to login"
       creds <- getLoginCredentials
       sec <- parseUrl starExecUrl
       let req1 = urlEncodedBody [ ("j_username", TE.encodeUtf8 $ user creds)
                           , ("j_password", TE.encodeUtf8 $ password creds) 
                           , ("cookieexists", "false")
                           ] 
                 $ sec { method = "POST"
                       , path = loginPath
                       }
       resp1 <- sendRequestRaw (req1, man, undefined)
       logWarnN  $ T.pack  $ "sendRequest: try again"
       sendRequestRaw (req0, man, undefined)

needs_login r =
     ( responseStatus r /= ok200)
  || ( BS.isInfixOf "Login - StarExec" $ BSL.toStrict $ responseBody r )

-- | this is under the same semaphore as sendRequest
getConnection :: Handler StarExecConnection
getConnection = runCon_exclusive $ do
  logWarnN  $ T.pack  $ "getConnection"
  SessionData cj d <- getSessionData
  sec <- parseUrl starExecUrl
  app <- getYesod
  return (sec, httpManager app, cj)

{-

-- | Why?
index :: StarExecConnection -> Handler StarExecConnection
index (sec, man, cookies) = do
  let req = sec { method = "GET"
                , path = indexPath
                }
  resp <- sendRequest (req, man, cookies)
  return (sec, man, responseCookieJar resp)

getLocation :: Response body -> Maybe BS.ByteString
getLocation resp = 
    let locs = filter (\(n,_) -> n == "Location" ) (responseHeaders resp)
    in
      if null locs then Nothing else Just $ snd $ head locs

checkLogin :: StarExecConnection -> Handler Bool
checkLogin (sec, man, cookies) = do
  let req = sec { method = "HEAD"
                , path = "starexec/secure/index.jsp"
                , redirectCount = 0
                , checkStatus = (\_ _ _ -> Nothing)
                }
  logWarnN $ T.pack $ "checkLogin ..."
  resp <- sendRequest (req, man, cookies)
  let answer = isLoggedIn $ getLocation resp
  logWarnN $ T.pack $ "checkLogin: " ++ show answer
  return answer

login :: StarExecConnection -> Login -> Handler StarExecConnection
login con@(sec, man, cookies) creds = do
  logWarnN $ T.pack $ "login ..."
  li <- checkLogin con
  if li
    then do
      logWarnN $ T.pack $ "we are alread logged in"
      return con
    else do
      logWarnN $ T.pack $ "we are not logged in"
      let req = urlEncodedBody [ ("j_username", TE.encodeUtf8 $ user creds)
                           , ("j_password", TE.encodeUtf8 $ password creds) 
                           , ("cookieexists", "false")
                           ] 
              $ sec { method = "POST"
                    , path = loginPath
                    }
      resp <- sendRequest (req, man, cookies)
      return (sec, man, responseCookieJar resp)

getConnection :: Handler StarExecConnection
getConnection = do
  logWarnN  $ T.pack  $ "getConnection"
  mSession <- getSessionCookies
  return (
  app <- getYesod ; let man = httpManager app
  con@(_, _, cookies) <- case mSession of
      Nothing -> do
        con' <- index (sec, man, createCookieJar [])
        creds <- getLoginCredentials
        con'' <- login con' creds
        return con''
      Just session -> do
        let date' = date session
            since = diffTime currentTime date'
            cookies = cookieData session
        if since < 3300.0
          then index (sec, man, cookies)
          else do
          con <- index (sec, man, createCookieJar [])
          creds <- getLoginCredentials
          login con creds
  logWarnN  $ T.pack  $ "getConnection - before write"
  writeSessionData' cookies currentTime
  logWarnN  $ T.pack  $ "getConnection - after  write"
  return con


-}
