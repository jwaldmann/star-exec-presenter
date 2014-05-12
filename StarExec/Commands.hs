{-
  StarExecCommands: Module that contains and handles all request to the
  starexec-cluster
-}

{-# LANGUAGE DeriveGeneric #-}

module StarExec.Commands
  ( login
  , logout
  , checkLogin
  , getConnection
  , getUserID
  , getSessionUserID
  , isSessionValid
  , listPrim
  ) where

import Import hiding (getSession)
import Prelude (head)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types.Header
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Resource.Internal
import Yesod.Core hiding (getSession)
import Data.Maybe
import StarExec.Types
import qualified Data.List as List
import Data.Char
import Data.Aeson
import GHC.Generics
import Text.XML
import Text.XML.Cursor

-- internals

type Cookies = [Cookie]
type StarExecConnection = (Request, Manager)
type PrimIdent = (Text, Text)

{-
aaData: [[,…], [,…]]
iTotalDisplayRecords: 2
iTotalRecords: 2
sEcho: 1
-}
data ListPrimResult = ListPrimResult
  { aaData :: ![[Text]]
  , iTotalDisplayRecords :: Int
  , iTotalRecords :: Int
  , sEcho :: Int
  } deriving (Show, Generic)

instance FromJSON ListPrimResult

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

primPath :: BS.ByteString
primPath = "starexec/services/space/{id}/{type}/pagination"

getPrimURL :: BS.ByteString -> [(String, String)] -> BS.ByteString
getPrimURL url patterns = List.foldl' (\path (pattern, sub) ->
    BSL.toStrict $ BSS.replace
      (BSC.pack pattern)
      (BSC.pack sub)
      path
  ) url patterns

getPostData :: Int -> [(BS.ByteString, BS.ByteString)]
--getPostData :: Int -> [(String, String)]
getPostData columns =
  [ ("sEcho", "1")
  , ("iColumns", BSC.pack $ show columns)
  , ("sColumns", "")
  , ("iDisplayStart", "0")
  , ("iDisplayLength", "2147483647")
  , ("iSortCol_0", "0")
  , ("sSearch", "")
  , ("sSortDir_0", "asc") ]

decodeUtf8Body :: Response BSL.ByteString -> Text
decodeUtf8Body = TE.decodeUtf8 . BSL.toStrict . responseBody

getInput :: Cursor -> Cursor
getInput c = head $ descendant c >>= element "input"

getNodeValue :: Cursor -> Text
getNodeValue = head . attribute "value"

getAnchor :: Cursor -> Cursor
getAnchor c = head $ descendant c >>= element "a" >>= child

wrapHtml :: TL.Text -> TL.Text
wrapHtml html = 
  let (+>) = TL.append
  in "<div>" +> html +> "</div>"

parseListPrimResult :: ( MonadIO m ) =>
  Int -> StarExecListType -> ListPrimResult -> m (Maybe [PrimIdent])
parseListPrimResult primID primType jsonObj = do
  let zipped = map (\[a,b] -> (a,b)) $ aaData jsonObj
      htmls = map (\(html, desc) ->
                  (parseText_ def $ wrapHtml $ TL.fromStrict html, desc))
                zipped
      infos = map (\(doc, desc) ->
                  let cursor = fromDocument doc
                      input = getInput cursor
                      anchor = getAnchor cursor
                      inputID = getNodeValue input
                      anchorContent = head $ content anchor
                  in (anchorContent, inputID))
                htmls
  return $ Just infos

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
  man <- withManager return
  index (sec, man)
  return (sec, man)

sendRequest :: ( MonadHandler m ) =>
  StarExecConnection -> m (Response BSL.ByteString)
sendRequest (req, man) = do
  cookies <- getSessionCookies
  let req' =  req { cookieJar = Just cookies }
  resp <- httpLbs req' man
  setSessionCookies $ responseCookieJar resp
  return resp

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
          let req = sec { method = "HEAD"
                        , path = "starexec/secure/index.jsp"
                        , redirectCount = 0
                        , checkStatus = (\_ _ _ -> Nothing)
                        }
          resp <- sendRequest (req, man)
          return $ isLoggedIn $ getLocation resp

index :: ( MonadHandler m, MonadIO m ) =>
  StarExecConnection -> m ()
index (sec, man) = do
  let req = sec { method = "GET"
                , path = indexPath
                }
  _ <- sendRequest (req, man)
  return ()

login :: ( MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> Text -> Text -> m Bool
login (sec, man) user pass = do
  let req = urlEncodedBody [ ("j_username", TE.encodeUtf8 user)
                           , ("j_password", TE.encodeUtf8 pass) 
                           , ("cookieexists", "false")
                           ] 
              $ sec { method = "POST"
                    , path = loginPath
                    }
  resp <- sendRequest (req, man)
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
  let req = sec { method = "POST"
                , path = logoutPath
                }
  -- resp :: Response Data.ByteString.Lazy.Internal.ByteString
  _ <- sendRequest (req, man)
  deleteSessionCookies
  deleteSession starExecUserID
  deleteSessionUserID
  return ()

getUserID :: (MonadHandler m, MonadIO m, MonadBaseControl IO m, MonadThrow m ) =>
  StarExecConnection -> m Text
getUserID (sec, man) = do
  let req = sec { method = "GET"
                , path = userIDPath
                }
  resp <- sendRequest (req, man)
  return $ decodeUtf8Body resp

listPrim :: ( MonadHandler m ) =>
  StarExecConnection -> Int -> StarExecListType -> Int -> m (Maybe [PrimIdent])
listPrim (sec, man) primID primType columns = do
  let sType = map toLower $ show primType
      reqPath = getPrimURL
              primPath
              [ ("{id}", show primID)
              , ("{type}", sType) ]
      (+>) = BS.append
      postData = foldl (\bs (k, v) ->
          bs +> "&" +> k +> "=" +> v
        ) "" $ getPostData columns
      req = sec {
                  method = "POST"
                , path = reqPath
                , requestHeaders = [ (
                    hContentType,
                    "application/x-www-form-urlencoded"
                  ) ]
                , requestBody = RequestBodyBS $ BS.tail postData
                }
  resp <- sendRequest (req, man)
  let jsonObj = eitherDecode
        $ responseBody resp :: (Either String ListPrimResult)
  mPrims <- case jsonObj of
                Right result ->
                  parseListPrimResult primID primType result
                Left msg -> do
                  liftIO $ print $ show msg
                  return Nothing
  return mPrims
