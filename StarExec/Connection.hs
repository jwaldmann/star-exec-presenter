module StarExec.Connection
    ( getConnection
    , sendRequest
    , index
    ) where

import Import
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BSL
import StarExec.Types
import StarExec.Urls
import StarExec.Session
import Control.Monad.Catch

index :: ( MonadHandler m, MonadIO m ) =>
  StarExecConnection -> m ()
index (sec, man) = do
  let req = sec { method = "GET"
                , path = indexPath
                }
  _ <- sendRequest (req, man)
  return ()

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