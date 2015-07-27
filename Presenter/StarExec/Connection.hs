module Presenter.StarExec.Connection
    ( sendRequest
    -- , index
    , getLoginCredentials
    , killmenothing
    ) where

import Import
import Prelude (head)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (ok200)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isHexDigit)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Presenter.StarExec.Urls
import Presenter.Auth ( getLoginCredentials )
import Presenter.Prelude (diffTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar
import Control.Concurrent.STM
import Control.Concurrent.MVar
-- import Control.Concurrent.SSem
import qualified Control.Concurrent.FairRWLock as Lock
import Control.Exception (throw)
import Control.Monad.Catch (bracket_)
import Control.Monad ((>=>), guard, when)
import Control.Monad.Logger
import Data.Maybe (listToMaybe)

import qualified Network.HTTP.Types.Header as H

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

setSessionData :: CookieJar -> Maybe BS.ByteString -> UTCTime -> Handler ()
setSessionData cj sid d = do
  app <- getYesod
  lift $ atomically $ writeTVar (sessionData app) $ SessionData cj sid d

-- | raw request. 
--  will silently set cookies to session state.
sendRequestRaw :: Bool -- ^ drop old session data
               -> Request
               -> Handler (Response BSL.ByteString)
sendRequestRaw dropold req0 = do
  man <- httpManager <$> getYesod
  SessionData cj sid d <- getSessionData
  -- https://github.com/snoyberg/http-client/issues/117
  let req =  req0 { cookieJar = Just
                      $ if dropold then createCookieJar [killmenothing] else cj 
                  , checkStatus = \ _ _ _ -> Nothing
                  , requestHeaders =
                    [ ( H.hAcceptLanguage, "en-US,en;q=0.5" )
                    -- , ( H.hConnection, "keep-alive" )
                    -- , ( H.hCookie , "killmenothing"
                    --   <> case sid of Nothing -> mempty ; Just i -> "; JSESSIONID=" <> i )
                    ]
                  }
  logWarnN  $ T.pack  $ "sendRequestRaw: " <> show (path req)
  logWarnN  $ T.pack  $ "sendRequestRaw: " <> show (cookieJar req)
  when False $ case requestBody req of
    RequestBodyLBS s ->
      logWarnN  $ T.pack  $ "sendRequestRaw: " <> show s
  start <- liftIO getCurrentTime
  resp <- httpLbs req man
  end <- liftIO getCurrentTime
  logWarnN  $ T.pack  $ "done sendRequestRaw: " <> show (path req)
                       <> "response status: " <> show (responseStatus resp)
                           <> "response cookies: " <> show (responseCookieJar resp)
         <> "time: " <> show (diffUTCTime end start)
  when False $ logWarnN $ T.pack $ show resp
  logWarnN $ T.pack $ "responseHeaders " <> show (responseHeaders resp)
  let sid' = -- getJsessionidFromHeaders $ responseHeaders resp
             getJsessionidFromCJ $ responseCookieJar resp
  logWarnN $ T.pack $ "current sid': " <> show sid'
  setSessionData (responseCookieJar resp) (case sid' of Nothing -> sid ; _ -> sid' ) end
  return resp



-- | managed requests: will execute Login if necessary.
sendRequest req0 = do
  logWarnN  $ T.pack  $ "sendRequest: " <> show (path req0)
  resp0 <- runCon_exclusive $ sendRequestRaw False $ req0 { redirectCount = 0 }
  if not $ needs_login resp0
     then do
       logWarnN  $ T.pack  $ "sendRequest: OK"
       return resp0
    else do    
       logWarnN  $ T.pack  $ "sendRequest: not OK, need to login"

       runCon_exclusive $ do        
         base <- parseUrl starExecUrl
         -- resp1 <- sendRequestRaw True $ base { method = "GET", path = indexPath, redirectCount = 0 }
         creds <- getLoginCredentials
         resp2 <- sendRequestRaw False
           $ urlEncodedBody [ ("j_username", TE.encodeUtf8 $ user creds)
                           , ("j_password", TE.encodeUtf8 $ password creds) 
                           , ("cookieexists", "false")
                           ] 
                 $ base { method = "POST" , path = loginPath
                        , redirectCount = 0
                        }      
         -- resp3 <- sendRequestRaw False $ base { method = "GET", path = indexPath, redirectCount = 0 }
         return ()

       logWarnN  $ T.pack  $ "repeat original sendRequest (RECURSE)"
       sendRequest req0

getJsessionidFromCJ cj = listToMaybe $ do
  c <- destroyCookieJar cj
  guard $ cookie_name c == "JSESSIONID"
  return $ cookie_value c
      
getJsessionidFromHeaders hs = listToMaybe $ do
        (k,v) <- hs
        guard $ k == "Set-Cookie"
        let js = "JSESSIONID=" 
        let (pre,post) = BS.splitAt (BS.length js) v
        guard $ pre == js
        return $ BSC.takeWhile isHexDigit post

needs_login r =
   ( responseStatus r /= ok200)
  ||   ( BS.isInfixOf "<title>Login - StarExec</title>" $ BSL.toStrict $ responseBody r )
--  || ( BS.isInfixOf "Invalid username or password" $ BSL.toStrict $ responseBody r )

-- this is terrible.
-- http://www.4guysfromrolla.com/webtech/082400-1.shtml

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


  
{-
getConnection = runCon_exclusive $ do
  logWarnN  $ T.pack  $ "getConnection"
  SessionData cj d <- getSessionData
  sec <- parseUrl starExecUrl
  app <- getYesod
  return (sec, httpManager app, cj)
-}


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
