module StarExec.Session where

import Prelude
import qualified Data.Text as T
import Data.Maybe
import StarExec.Types
import Network.HTTP.Conduit
import Yesod.Core hiding (getSession)

starExecSessionID :: T.Text
starExecSessionID = "SESESSION"

starExecUserID :: T.Text
starExecUserID = "SEUSERID"

getSession :: MonadHandler m => m (Maybe Cookies)
getSession = do
  session <- lookupSession starExecSessionID
  case session of
        Nothing -> return Nothing
        Just sCookies -> return $ Just $ parseCookies sCookies

parseCookies :: T.Text -> Cookies
parseCookies = read . T.unpack

packCookies :: Cookies -> T.Text
packCookies = T.pack . show

setSessionCookies :: ( MonadHandler m ) => CookieJar -> m ()
setSessionCookies cookies =
  setSession starExecSessionID $ packCookies $ destroyCookieJar cookies

deleteSessionCookies :: ( MonadHandler m ) => m ()
deleteSessionCookies = deleteSession starExecSessionID

getSessionCookies :: ( MonadHandler m ) => m CookieJar
getSessionCookies = do
  session <- getSession
  return $ createCookieJar $ fromMaybe [] session

-- StarExec UserID

getSessionUserID :: ( MonadHandler m ) => m (Maybe T.Text)
getSessionUserID = lookupSession starExecUserID

setSessionUserID :: ( MonadHandler m ) => T.Text -> m ()
setSessionUserID userID = setSession starExecUserID userID

deleteSessionUserID :: ( MonadHandler m ) => m ()
deleteSessionUserID = deleteSession starExecUserID

hasValidSession :: ( MonadHandler m ) => m Bool
hasValidSession = do
  userID <- getSessionUserID
  return $ if userID /= Nothing then True else False