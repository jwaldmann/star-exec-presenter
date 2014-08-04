module StarExec.Connection
    ( getConnection
    , sendRequest
    , index
    , getLoginCredentials
    ) where

import Import
import Prelude (head, readFile, writeFile)
import System.Directory
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
--import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import StarExec.Types
import StarExec.Urls
import StarExec.Auth ( getLoginCredentials )
import Data.Time.Clock
import Control.Concurrent.STM

getSessionData' :: Handler (Maybe SessionData)
getSessionData' = do
  app <- getYesod
  lift $ atomically $ readTVar $ sessionData app

writeSessionData' :: CookieJar -> UTCTime -> Handler ()
writeSessionData' cj d = do
  app <- getYesod
  let session = SessionData cj d
      appSession = sessionData app
  lift $ atomically $ writeTVar appSession $ Just session

getSessionData :: Handler (Maybe StarExecSessionData)
getSessionData = do
  mSessionData <- runDB $ getBy $ UniqueStarExecSessionData 0
  case mSessionData of
    Nothing -> return Nothing
    Just en -> return $ Just $ entityVal en

writeSessionData :: CookieJar -> UTCTime -> Handler ()
writeSessionData cj d = do
  let ct = T.pack $ show cj
  runDB $ do
    mPersistSessionData <- getBy $ UniqueStarExecSessionData 0
    case mPersistSessionData of
      Just en ->
        update (entityKey en) [StarExecSessionDataCookies =. ct]
      Nothing -> do
        _ <- insertUnique $ StarExecSessionData 0 ct d
        return ()

user :: Login -> Text
user (Login u _) = u

password :: Login -> Text
password (Login _ pass) = pass

isLoggedIn :: Maybe BS.ByteString -> Bool
isLoggedIn Nothing = False
isLoggedIn (Just loc) = if loc == starExecSpacesPath
                          then True
                          else False

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
  resp <- sendRequest (req, man, cookies)
  return $ isLoggedIn $ getLocation resp

login :: StarExecConnection -> Login -> Handler StarExecConnection
login con@(sec, man, cookies) creds = do
  _ <- checkLogin con
  let req = urlEncodedBody [ ("j_username", TE.encodeUtf8 $ user creds)
                           , ("j_password", TE.encodeUtf8 $ password creds) 
                           , ("cookieexists", "false")
                           ] 
              $ sec { method = "POST"
                    , path = loginPath
                    }
  resp <- sendRequest (req, man, cookies)
  return (sec, man, responseCookieJar resp)

index :: StarExecConnection -> Handler StarExecConnection
index (sec, man, cookies) = do
  let req = sec { method = "GET"
                , path = indexPath
                }
  resp <- sendRequest (req, man, cookies)
  return (sec, man, responseCookieJar resp)

getConnection :: Handler StarExecConnection
getConnection = do
  --liftIO $ putStrLn "getConnection >>>"
  mSession <- getSessionData'
  currentTime <- liftIO getCurrentTime
  sec <- parseUrl starExecUrl
  man <- withManager return
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
  writeSessionData' cookies currentTime
  --liftIO $ putStrLn "<<< getConnection"
  return con

sendRequest :: StarExecConnection -> Handler (Response BSL.ByteString)
sendRequest (req, man, cookies) = do
  let req' =  req { cookieJar = Just cookies }
  httpLbs req' man
