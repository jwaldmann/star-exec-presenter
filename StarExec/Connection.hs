module StarExec.Connection
    ( getConnection
    , sendRequest
    , index
    ) where

import Import
import Prelude (head, readFile, writeFile)
import System.Directory
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import StarExec.Types
import StarExec.Urls
import Control.Monad.Catch
import Data.Time.Clock

getLoginCredentials :: ( MonadIO m ) => m Login
getLoginCredentials = liftIO $ do
  home <- getHomeDirectory
  slogin <- readFile $ home ++ "/.star_exec"
  return $ read slogin

getSessionData :: (MonadIO m) => m (Maybe SessionData)
getSessionData = liftIO $ do
  home <- getHomeDirectory
  let filePath = home ++ "/.star_exec_session"
  dataExists <- doesFileExist filePath
  if dataExists
    then do
      sData <- readFile filePath
      return $ Just $ read sData
    else return Nothing

writeSessionData :: ( MonadIO m ) => CookieJar -> UTCTime -> m ()
writeSessionData cookieJar date = liftIO $ do
  home <- getHomeDirectory
  let file = show $ SessionData { cookieData = T.pack $ show cookieJar
                                , date = date }
  writeFile (home ++ "/.star_exec_session") file

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

checkLogin :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> m Bool
checkLogin (sec, man, cookies) = do
  let req = sec { method = "HEAD"
                , path = "starexec/secure/index.jsp"
                , redirectCount = 0
                , checkStatus = (\_ _ _ -> Nothing)
                }
  resp <- sendRequest (req, man, cookies)
  return $ isLoggedIn $ getLocation resp

login :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> Login -> m StarExecConnection
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
  --loggedIn <- checkLogin con
  --if loggedIn
  --  then return (sec, man, responseCookieJar resp)
  --  else return $ error "Wrong login-credentials"

index :: ( MonadHandler m, MonadIO m ) =>
  StarExecConnection -> m StarExecConnection
index (sec, man, cookies) = do
  let req = sec { method = "GET"
                , path = indexPath
                }
  resp <- sendRequest (req, man, cookies)
  return (sec, man, responseCookieJar resp)

getConnection :: ( MonadHandler m, MonadBaseControl IO m ) =>
  m StarExecConnection
getConnection = do
  mSession <- getSessionData
  currentTime <- liftIO getCurrentTime
  sec <- parseUrl starExecUrl
  man <- withManager return
  con@(req, man, cookies) <- case mSession of
    Nothing -> do
      con <- index (sec, man, createCookieJar [])
      creds <- getLoginCredentials
      (req, man, cookies) <- login con creds
      return (req, man, cookies)
    Just session -> do
      let diffTime = fromRational $ toRational $ diffUTCTime currentTime $ date session :: Double
          cookies = read $ T.unpack $ cookieData session
      if diffTime < 300.0
        then index (sec, man, cookies)
        else do
          con <- index (sec, man, createCookieJar [])
          creds <- getLoginCredentials
          login con creds
  writeSessionData cookies currentTime
  return con

sendRequest :: ( MonadHandler m ) =>
  StarExecConnection -> m (Response BSL.ByteString)
sendRequest (req, man, cookies) = do
  let req' =  req { cookieJar = Just cookies }
  httpLbs req' man
  --return resp
