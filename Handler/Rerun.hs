{-# language DoAndIfThenElse #-}

module Handler.Rerun where

import Import
import Yesod.Auth
import Data.Maybe
import Presenter.StarExec.Commands

getRerunR :: JobIds -> Handler Html
getRerunR jobIds = do
  maid <- maybeAuthId
  if isJust maid then do
    rerunJobs $ getIds jobIds
    defaultLayout [whamlet|
                   <h1>Jobs re-running
                   <pre>#{show jobIds}
    |]
  else do
    defaultLayout [whamlet|
                   <h1>not authorized
    |]
