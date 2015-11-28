module Presenter.Model.Types where

import Prelude
import Data.Text (Text)
import Network.HTTP.Conduit (Request, Manager, CookieJar, Cookie)
import Data.Time.Clock (UTCTime)
import Data.ByteString as BS

type Email = Text
type Password = Text
type Name = Text
type Description = Text
type Rank = Int
type Score = Int
type Seconds = Double
type PostProcId = Int

data Login = Login Email Password deriving (Show, Read, Eq)

user :: Login -> Email
user (Login u _) = u

password :: Login -> Password
password (Login _ p) = p

data SessionData = SessionData
  { cookieData :: CookieJar
  , jsessionid :: Maybe BS.ByteString
  , date :: UTCTime
  } deriving (Show, Read)

type Cookies = [Cookie]

type StarExecConnection = (Request, Manager, CookieJar)
