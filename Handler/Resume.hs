{-# language DoAndIfThenElse #-}

module Handler.Resume where

import Import
import Yesod.Auth
import Data.Maybe
import Presenter.StarExec.Commands

postResumeR :: JobIds -> Handler Html
postResumeR jobIds = do
  maid <- maybeAuthId
  if isJust maid then do
    resumeJobs $ getIds jobIds
    defaultLayout [whamlet|
                   <h1>Jobs resumed
                   <pre>#{show jobIds}
    |]
  else do
    defaultLayout [whamlet|
                   <h1>not authorized
    |]


