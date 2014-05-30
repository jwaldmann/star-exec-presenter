module StarExec.Connection
    ( getConnection
    , sendRequest
    , index
    ) where

import Import
import Prelude (head, readFile)
import System.Directory
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import StarExec.Types
import StarExec.Urls
import StarExec.Session
import Control.Monad.Catch

getLoginCredentials :: ( MonadIO m ) => m Login
getLoginCredentials = liftIO $ do
  home <- getHomeDirectory
  slogin <- readFile $ home ++ "/.star_exec"
  return $ read slogin

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
  --liftIO $ putStrLn $ show req
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

getConnection :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  m StarExecConnection
getConnection = do
  sec <- parseUrl starExecUrl
  man <- withManager return
  liftIO $ putStrLn "requesting index-page"
  con <- index (sec, man, createCookieJar [])
  creds <- getLoginCredentials
  liftIO $ putStrLn "performing login"
  login con creds
  --return (sec, man)

sendRequest :: ( MonadHandler m ) =>
  StarExecConnection -> m (Response BSL.ByteString)
sendRequest (req, man, cookies) = do
  let req' =  req { cookieJar = Just cookies }
  resp <- httpLbs req' man
  setSessionCookies $ responseCookieJar resp
  return resp
