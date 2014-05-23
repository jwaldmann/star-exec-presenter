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
  , getJobInfo
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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Resource.Internal
import Yesod.Core hiding (getSession)
import Data.Maybe
import StarExec.Types
import StarExec.Urls
import StarExec.Session
import StarExec.Connection
import qualified Data.List as List
import Data.Char
import Data.Aeson
import Data.Either
import Text.XML
import Text.XML.Cursor
import qualified Codec.Archive.Zip as Zip
import qualified Data.Csv as CSV
import qualified Data.Vector as Vector

-- internals

decodeUtf8Body :: Response BSL.ByteString -> Text
decodeUtf8Body = TE.decodeUtf8 . BSL.toStrict . responseBody

-- internal Methods

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

-- StarExec Session-Cookies

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

decodeCSV ::
  BSL.ByteString -> Either String (CSV.Header, (Vector.Vector JobResultInfo))
decodeCSV csv = CSV.decodeByName csv

getJobInfo :: ( MonadHandler m ) =>
 StarExecConnection -> Int -> m (Maybe [JobResultInfo])
getJobInfo (sec, man) jobId = do
  let (+>) = BS.append
      req = sec { method = "GET"
                , path = downloadPath
                , queryString = "id=" +> (BSC.pack $ show jobId)
                                +> "&type=job&returnids=true"
                }
  resp <- sendRequest (req, man)
  let archive = Zip.toArchive $ responseBody resp
  jobs <- case Zip.zEntries archive of
            entry:entries -> do
              let eitherVector = decodeCSV $ Zip.fromEntry entry
              case eitherVector of
                Left msg -> do
                  liftIO $ print msg
                  return Nothing
                Right (header, jobInfos) ->
                  return $ Just $ Vector.toList jobInfos
            [] -> return Nothing
  return jobs
