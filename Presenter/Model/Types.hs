module Presenter.Model.Types where

import Prelude
import Data.Text (Text)
import Network.HTTP.Conduit (Request, Manager, CookieJar, Cookie)
import Data.Time.Clock (UTCTime)

type Email = Text
type Password = Text
type Name = Text
type Description = Text
type Rank = Int
type Score = Int
type Seconds = Double
type PostProcId = Int

data Login = Login Email Password deriving (Show, Read, Eq)

data SessionData = SessionData
  { cookieData :: CookieJar
  , date :: UTCTime
  } deriving (Show, Read)

{-
-}
type Cookies = [Cookie]

{-
-}
type StarExecConnection = (Request, Manager, CookieJar)
