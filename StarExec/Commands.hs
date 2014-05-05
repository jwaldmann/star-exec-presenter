{-
  StarExecCommands: Module that contains and handles all request to the
  starexec-cluster
-}

module StarExec.Commands
  ( login
  , logout
  , checkLogin
  , getConnection
  , getUserID
  , getSessionUserID
  , isSessionValid
  --, getSessionCookies
  --, setSessionCookies
  --, deleteSessionCookies
  ) where

import Import hiding (getSession)
import Prelude (head)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Resource.Internal
import Yesod.Core hiding (getSession)
import Data.Maybe

-- internals

type RequestHandler m b = Request -> Manager -> ResourceT m b
type Cookies = [Cookie]
type StarExecConnection = (Request, Manager)

-- Statics

starExecSessionID :: Text
starExecSessionID = "SESESSION"

starExecUserID :: Text
starExecUserID = "SEUSERID"

starExecUrl :: String
starExecUrl = "https://www.starexec.org/"

starExecSpacesPath :: BS.ByteString
starExecSpacesPath = "https://www.starexec.org/starexec/secure/explore/spaces.jsp"

indexPath :: BS.ByteString
indexPath = "starexec/secure/index.jsp"

loginPath :: BS.ByteString
loginPath = "starexec/secure/j_security_check"

logoutPath :: BS.ByteString
logoutPath = "starexec/services/session/logout"

spacesPath :: BS.ByteString
spacesPath = "starexec/secure/explore/spaces.jsp"

userIDPath :: BS.ByteString
userIDPath = "starexec/services/users/getid"

-- internal Methods

getSession :: MonadHandler m => m (Maybe Cookies)
getSession = do
  session <- lookupSession starExecSessionID
  case session of
        Nothing -> return Nothing
        Just sCookies -> return $ Just $ parseCookies sCookies

isLoggedIn :: Maybe BS.ByteString -> Bool
isLoggedIn Nothing = False
isLoggedIn (Just loc) = if loc == starExecSpacesPath
                          then True
                          else False

parseCookies :: Text -> Cookies
parseCookies = read . T.unpack

packCookies :: Cookies -> Text
packCookies = T.pack . show

sendRequest :: ( MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  (RequestHandler m b) -> m b
sendRequest secFunc = do
  sec <- parseUrl starExecUrl
  withManager $ secFunc sec

getLocation :: Response body -> Maybe BS.ByteString
getLocation resp = 
    let locs = filter (\(n,_) -> n == "Location" ) (responseHeaders resp)
    in
      if null locs then Nothing else Just $ snd $ head locs

-- external Methods

getConnection :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  m StarExecConnection
getConnection = do
  sec <- parseUrl starExecUrl
  cookies <- getSessionCookies
  man <- withManager return
  newCookies <- index (sec, man) cookies
  setSessionCookies newCookies
  return (sec, man)

-- StarExec Session-Cookies

getSessionCookies :: ( MonadHandler m ) => m CookieJar
getSessionCookies = do
  session <- getSession
  return $ createCookieJar $ fromMaybe [] session

setSessionCookies :: ( MonadHandler m ) => CookieJar -> m ()
setSessionCookies cookies =
  setSession starExecSessionID $ packCookies $ destroyCookieJar cookies

deleteSessionCookies :: ( MonadHandler m ) => m ()
deleteSessionCookies = deleteSession starExecSessionID

-- StarExec UserID

getSessionUserID :: ( MonadHandler m ) => m (Maybe Text)
getSessionUserID = lookupSession starExecUserID
  --do
  --userID <- lookupSession starExecUserID
  --return userID

setSessionUserID :: ( MonadHandler m ) => Text -> m ()
setSessionUserID userID = setSession starExecUserID userID

deleteSessionUserID :: ( MonadHandler m ) => m ()
deleteSessionUserID = deleteSession starExecUserID

isSessionValid :: ( MonadHandler m ) => m Bool
isSessionValid = do
  userID <- getSessionUserID
  return $ if userID /= Nothing then True else False

checkLogin :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> m Bool
checkLogin (sec, man) = do
  session <- getSession
  case session of
        Nothing -> return False
        Just cookies -> do
          --newCookies <- index (sec, man) $ createCookieJar cookies
          let req = sec { method = "HEAD"
                        , path = "starexec/secure/index.jsp"
                        , cookieJar = Just $ createCookieJar cookies
                        , redirectCount = 0
                        , checkStatus = (\_ _ _ -> Nothing)
                        }
          resp <- httpLbs req man
          setSessionCookies $ responseCookieJar resp
          return $ isLoggedIn $ getLocation resp

index :: MonadIO m => StarExecConnection -> CookieJar -> m CookieJar
index (sec, man) cookies = do
  let req = sec { method = "GET"
                , path = indexPath
                , cookieJar = Just cookies
                }
  -- resp :: Response Data.ByteString.Lazy.Internal.ByteString
  resp <- httpLbs req man
  let respCookies = responseCookieJar resp
  return respCookies

login :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> Text -> Text -> m Bool
login (sec, man) user pass = do
  cookies <- getSessionCookies
  let req = urlEncodedBody [ ("j_username", TE.encodeUtf8 user)
                           , ("j_password", TE.encodeUtf8 pass) 
                           , ("cookieexists", "false")
                           ] 
              $ sec { method = "POST"
                    , path = loginPath
                    , cookieJar = Just cookies
                    }
  -- resp :: Response Data.ByteString.Lazy.Internal.ByteString
  resp <- httpLbs req man
  let respCookies = responseCookieJar resp
  setSessionCookies respCookies
  loggedIn <- checkLogin (sec, man)
  if loggedIn
    then do
      userID <- getUserID (sec, man)
      setSessionUserID userID
    else deleteSessionUserID
  return loggedIn

logout :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> m ()
logout (sec, man) = do
  cookies <- getSessionCookies
  let req = sec { method = "POST"
                , path = logoutPath
                , cookieJar = Just cookies
                }
  -- resp :: Response Data.ByteString.Lazy.Internal.ByteString
  resp <- httpLbs req man
  deleteSessionCookies
  deleteSession starExecUserID
  deleteSessionUserID
  return ()

getUserID :: (MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection  -> m Text
getUserID (sec, man) = do
  cookies <- getSessionCookies
  let req = sec { method = "GET"
                , path = userIDPath
                , cookieJar = Just cookies
                }
  -- resp :: Response Data.ByteString.Lazy.Internal.ByteString
  resp <- httpLbs req man
  let respCookies = responseCookieJar resp
  setSessionCookies respCookies
  return $ TE.decodeUtf8 $ BSL.toStrict $ responseBody resp
