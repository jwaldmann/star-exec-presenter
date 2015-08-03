{-# language DoAndIfThenElse #-}

module Handler.Pause where

import Import
import Yesod.Auth
import Data.Maybe
import Presenter.StarExec.Commands

getPauseR :: JobIds -> Handler Html
getPauseR jobIds = do
  maid <- maybeAuthId
  if isJust maid then do
    pauseJobs $ getIds jobIds
    defaultLayout [whamlet|
                   <h1>Jobs paused
                   <pre>#{show jobIds}
    |]
  else do
    defaultLayout [whamlet|
                   <h1>not authorized
    |]
